{-# LANGUAGE NamedFieldPuns #-}
import           Protolude                            hiding (Text, local)
import           Protolude.Partial                    (read)

import           Data.Aeson
import           Data.Cache.LRU.IO                    (AtomicLRU)
import qualified Data.Cache.LRU.IO                    as Cache
import           Data.UUID                            (UUID)
import qualified Data.UUID                            as UUID
import qualified Data.UUID.V4                         as UUID
import qualified Data.Set                             as Set
import qualified Data.Text                            as StrictText
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as Text
import qualified Network.HTTP.Types.Status            as Sc
import qualified Network.Wai.Middleware.Gzip          as Sc
import qualified Network.Wai.Handler.WebSockets       as WaiWs
import qualified Network.WebSockets                   as WS
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai (logStdout)
import qualified System.Environment                   as Sys
import qualified Web.Scotty                           as Sc
import qualified Web.Scotty.Cookie                    as Sc

import           Data.Types

type RoomCache = AtomicLRU RoomID RoomState

data RoomPost = CreateRoom { roomNameParam :: Text, username :: Text }
              | JoinRoom   { roomIDParam :: RoomID, username :: Text }
              deriving (Eq, Ord, Read, Show)

userCookie :: UUID -> StrictText.Text
userCookie rid = "username-" <> (UUID.toText rid)

main :: IO ()
main = do
  envport <- Sys.lookupEnv "PORT"
  let port = case envport of
        Nothing -> 80
        Just p  -> read p
  let settings = Warp.setPort port Warp.defaultSettings
  cache <- Cache.newAtomicLRU Nothing
  sapp  <- scottyApp cache
  let wsapp = websockApp cache
  Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions wsapp sapp

scottyApp :: RoomCache -> IO Wai.Application
scottyApp cache =
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip (Sc.def { Sc.gzipFiles = Sc.GzipCompress }) . Wai.logStdout

    Sc.get "/" $
      Sc.file "index.html"
    Sc.post "/" $ do
      params <- Sc.params
      case mkRoomPost params of
        Nothing  -> Sc.status Sc.badRequest400
        Just req -> do
          case req of
            cr@CreateRoom{} -> createRoom cr cache
            jr@JoinRoom{}   -> do
              let rid = unRoomID . roomIDParam $ jr
              Sc.setSimpleCookie (userCookie rid) (Text.toStrict $ username req)
              Sc.redirect $ "/room/" <> uuidToText rid
    Sc.get "/static/cookie-policy/"  $ Sc.file "static/cookie-policy.html"
    Sc.get "/static/privacy-policy/" $ Sc.file "static/privacy.html"
    -- TODO: Apply some simple scheme to avoid spam creation of rooms
    Sc.get "/room/:pRoomID"    $ Sc.file "room.html"
    -- TODO: setup some static middleware
    Sc.get "/static/client.js" $ Sc.file "static/client.js"
    Sc.get "/static/invite.js" $ Sc.file "static/invite.js"
    Sc.get "/static/style.css" $ Sc.file "static/style.css"

websockApp :: RoomCache -> WS.ServerApp
websockApp cache pending = do
  putText "ws connected"
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30 -- TODO: remove this deprecated dill

  roomidReq <- WS.receiveData conn
  case decode roomidReq of
    Nothing -> WS.sendClose conn ("Bad UUID byyeeeeeeee" :: Text)
    Just ClientInit{room, attendee} -> do
      maybeRoom <- Cache.lookup room cache
      case maybeRoom of
        Nothing       -> WS.sendClose conn ("byyyyyyyeeeee" :: Text)
        Just initRoom -> do
          let initRoom' = newUser attendee initRoom
          Cache.insert room initRoom' cache

          _ <- createReader attendee room conn cache
          -- this intentionally uses initRoom instead of initRoom'
          -- to force the first push of the state of the room
          _ <- iterate_ initRoom $ \rstate -> do
            curState <- pushWorld room conn cache rstate
            threadDelay 100000
            pure curState
          pure ()

    Just _ -> WS.sendClose conn ("Bad payload byyyeee" :: Text)

mkRoomPost :: [Sc.Param] -> Maybe RoomPost
mkRoomPost params = maybeJoin <|> maybeCreate
  where
  maybeJoin    = do
    roomIDP  <- param "room-id" params
    rID      <- textToRoomID roomIDP
    username <- param "username" params
    pure $ JoinRoom rID username
  maybeCreate  = do
    roomName  <- param "room-name" params
    username  <- param "username" params
    pure $ CreateRoom roomName username

createRoom :: RoomPost -> RoomCache -> Sc.ActionM ()
createRoom req cache = do
  rstate <- Sc.liftAndCatchIO $ do
    rID   <- fmap RoomID UUID.nextRandom
    let rstate = newRoom rID . roomNameParam $ req
    Cache.insert rID rstate cache
    putLText $ "Created room with ID [" <> roomIDToText rID <> "]"
    pure rstate

  let rid   = unRoomID $ roomID rstate
  Sc.setSimpleCookie (userCookie rid) (Text.toStrict $ username req)
  Sc.redirect $ "/room/" <> uuidToText rid

param :: Text -> [Sc.Param] -> Maybe Text
param var = fmap snd . find (\(par, _) -> par == var)

pushWorld :: RoomID -> WS.Connection -> RoomCache -> RoomState -> IO RoomState
pushWorld rid conn cache prevState = do
  maybeRoom <- Cache.lookup rid cache
  case maybeRoom of
    Nothing   -> WS.sendClose conn ("byyyyyyyeeeee" :: Text) >> pure prevState
    Just room -> do
      when (room /= prevState) $ WS.sendTextData conn . encode $ WORLD {
        attendees = Set.toList (users room),
        local     = toList (rlocal  room),
        broad     = toList (rglobal room),
        name      = roomName room
      }
      pure room

createReader :: UserID -> RoomID -> WS.Connection -> RoomCache -> IO ThreadId
createReader attendee rid conn cache = forkIO . forever $ do
  msg <- WS.receiveData conn
  maybeRoom <- Cache.lookup rid cache
  case maybeRoom of
    Nothing       -> WS.sendClose conn ("Room is closed byyyyee" :: Text)
    Just room -> do
      case decode msg of
        Nothing -> WS.sendClose conn ("Bad UUID byyeeeeeeee" :: Text)
        Just QUEUE{stack} -> do
            let room' = (case stack of
                           LOCAL -> qLocal
                           _     -> qGlobal) attendee room
            Cache.insert rid room' cache
        Just NEXT{stack} -> do
            let room' = (case stack of
                            LOCAL -> nextLocal
                            _     -> nextGlobal) room
            Cache.insert rid room' cache
        Just LEAVE       -> do
          let room' = deleteUser attendee room
          Cache.insert rid room' cache
          WS.sendClose conn ("Thanks for joining!" :: Text)
        _       -> WS.sendClose conn ("Unsupported byyyyeee" :: Text)

iterate_ :: Monad f => a -> (a -> f a) -> f a
iterate_ a f = f a >>= \a' -> iterate_ a' f
