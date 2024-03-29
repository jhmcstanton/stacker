{-# LANGUAGE NamedFieldPuns #-}
import           Prelude                                     (String, read)
import           Protolude                            hiding (local)

import           Data.Aeson
import           Data.Cache.LRU.IO                    (AtomicLRU)
import qualified Data.Cache.LRU.IO                    as Cache
import           Data.UUID                            (UUID)
import qualified Data.UUID                            as UUID
import qualified Data.UUID.V4                         as UUID
import           Data.String                          (fromString)
import qualified Data.Text.Lazy                       as LazyText
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

userCookie :: UUID -> Text
userCookie rid = "username-" <> UUID.toText rid

main :: IO ()
main = do
  port <- sysWithDefault "PORT" read 8080
  host <- sysWithDefault "HOST" fromString "*4"
  keepAliveURL <- Sys.lookupEnv "KEEPALIVE_URL"
  let settings = Warp.setHost host $ Warp.setPort port Warp.defaultSettings
  cache <- Cache.newAtomicLRU Nothing
  sapp  <- scottyApp cache keepAliveURL
  let wsapp = websockApp cache
  Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions wsapp sapp

scottyApp :: RoomCache -> Maybe String -> IO Wai.Application
scottyApp cache keepAlive =
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
              Sc.setSimpleCookie (userCookie rid) (username req)
              Sc.redirect $ "/room/" <> LazyText.fromStrict (toText rid)
    Sc.get "/static/cookie-policy/"  $ Sc.file "static/cookie-policy.html"
    Sc.get "/static/privacy-policy/" $ Sc.file "static/privacy.html"
    -- TODO: Apply some simple scheme to avoid spam creation of rooms
    Sc.get "/room/:pRoomID"    $ Sc.file "room.html"
    -- TODO: setup some static middleware
    Sc.get "/static/client.js" $ do
      Sc.addHeader "Content-Type" "text/javascript"
      Sc.file "static/client.js"
    Sc.get "/static/invite.js" $ do
      Sc.addHeader "Content-Type" "text/javascript"
      Sc.file "static/invite.js"
    Sc.get "/static/style.css" $ do
      Sc.addHeader "Content-Type" "text/css"
      Sc.file "static/style.css"

    case keepAlive of
      Nothing  -> pure ()
      Just url -> do
        Sc.get (Sc.capture url) $ Sc.file "static/keepalive.html"
        Sc.get "/static/keepalive.js" $ do
          Sc.addHeader "Content-Type" "text/javascript"
          Sc.file "static/keepalive.js"

websockApp :: RoomCache -> WS.ServerApp
websockApp cache pending = do
  putText "ws connected"
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30 -- TODO: remove this deprecated dill

  maybeRooms <- joinRoom conn cache
  case maybeRooms of
    Nothing -> pure ()
    Just (initRoom, initRoom', CLIENTINIT{room, attendee}) -> do
      _ <- Cache.insert room initRoom' cache
      _ <- pushWorld room conn cache nilRoom

      _ <- createReader attendee room conn cache
      -- this intentionally uses initRoom instead of initRoom'
      -- to force the first push of the state of the room
      _ <- iterate_ initRoom $ \rstate -> do
        curState <- pushWorld room conn cache rstate
        threadDelay 100000
        pure curState
      pure ()
    Just _ -> WS.sendClose conn ("Error while joining room" :: Text)

joinRoom :: WS.Connection -> RoomCache -> IO (Maybe (RoomState, RoomState, Payload))
joinRoom conn cache = do
  roomidReq <- WS.receiveData conn
  case decode roomidReq of
    Nothing -> WS.sendClose conn ("Bad UUID byyeeeeeeee" :: Text) >> pure Nothing
    Just cli@CLIENTINIT{room, attendee} -> do
      maybeRoom <- Cache.lookup room cache
      case maybeRoom of
        Nothing       -> WS.sendClose conn ("byyyyyyyeeeee" :: Text) >> pure Nothing
        Just initRoom -> do
          case newUser attendee initRoom of
            Just initRoom' -> pure $ pure (initRoom, initRoom', cli)
            Nothing        -> do
              _ <- WS.sendTextData conn . encode $ DUPLICATEUSER (userList initRoom)
              joinRoom conn cache
    Just _ -> WS.sendClose conn ("Bad payload byyyeee" :: Text) >> pure Nothing

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
    putText $ "Created room with ID [" <> roomIDToText rID <> "]"
    pure rstate

  let rid   = unRoomID $ roomID rstate
  Sc.setSimpleCookie (userCookie rid) (username req)
  Sc.redirect $ "/room/" <> LazyText.fromStrict (toText rid) <> "#facilitator"

param :: Text -> [Sc.Param] -> Maybe Text
param var = fmap snd . find (\(par, _) -> par == var)

pushWorld :: RoomID -> WS.Connection -> RoomCache -> RoomState -> IO RoomState
pushWorld rid conn cache prevState = do
  maybeRoom <- Cache.lookup rid cache
  case maybeRoom of
    Nothing   -> WS.sendClose conn ("byyyyyyyeeeee" :: Text) >> pure prevState
    Just room -> do
      when (room /= prevState) $ WS.sendTextData conn . encode $ WORLD {
        attendeeToCount = users room,
        local           = toList (rlocal  room),
        broad           = toList (rglobal room),
        name            = roomName room
      }
      pure room

createReader :: UserID -> RoomID -> WS.Connection -> RoomCache -> IO ThreadId
createReader attendee rid conn cache = forkIO . forever $ do
  msg       <- WS.receiveData conn
  maybeRoom <- Cache.lookup rid cache
  case maybeRoom of
    Nothing       -> WS.sendClose conn ("Room is closed byyyyee" :: Text)
    Just room -> do
      case decode msg of
        Nothing -> WS.sendClose conn ("Bad UUID byyeeeeeeee" :: Text)
        Just QUEUE{stack} -> do
            let room' = q stack attendee room
            Cache.insert rid room' cache
        Just QOTHER{stack, other} -> do
            let room' = q stack other room
            Cache.insert rid room' cache
        Just NEXT{stack} -> do
          case stack of
            LOCAL -> Cache.insert rid (nextLocal room) cache
            BROAD ->
              if lengthLocal room > 1
              then pure () -- Do nothing, don't delete an incomplete local stack
              else case peakNextGlobal room of
                     Nothing          -> pure () -- Nothing in global to mess with
                     Just nextSpeaker -> do
                       let room' = qLocal nextSpeaker . nextLocal . nextGlobal $ room
                       Cache.insert rid room' cache
        Just REORDER{stack, newstack} -> do
          let room' = reorder newstack stack room
          Cache.insert rid room' cache
        Just CANCEL{stack, newstack}  -> do
          let room' = cancelStack newstack stack room
          Cache.insert rid room' cache
        Just ADD{other} -> do
          case newUser other room of
            Nothing    -> pure ()
            Just room' -> Cache.insert rid room' cache
        Just LEAVE       -> do
          let room' = deleteUser attendee room
          Cache.insert rid room' cache
          WS.sendClose conn ("Thanks for joining!" :: Text)
        Just CLOSE       -> do
          _ <- Cache.delete rid cache
          WS.sendClose conn ("Thanks for joining!" :: Text)
        _       -> WS.sendClose conn ("Unsupported byyyyeee" :: Text)

iterate_ :: Monad f => a -> (a -> f a) -> f a
iterate_ a f = f a >>= \a' -> iterate_ a' f

sysWithDefault :: String -> (String -> a) -> a -> IO a
sysWithDefault var f x = fromMaybe x . fmap f <$> Sys.lookupEnv var
