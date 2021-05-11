import           Protolude                            hiding (Text)
import           Protolude.Error

import           Data.Cache.LRU.IO                    (AtomicLRU)
import qualified Data.Cache.LRU.IO                    as Cache
import           Data.UUID                            (UUID)
import qualified Data.UUID                            as UUID
import qualified Data.UUID.V4                         as UUID
import           Data.Sequence                        (Seq)
import qualified Data.Sequence                        as Seq
import qualified Data.Text                            as StrictText
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as Text
import qualified Web.Scotty                           as Sc
import qualified Web.Scotty.Cookie                    as Sc
import qualified Network.HTTP.Types.Status            as Sc
import qualified Network.Wai.Middleware.Gzip          as Sc
import qualified Network.Wai.Handler.WebSockets       as WaiWs
import qualified Network.WebSockets                   as WS
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai (logStdout)

import           Data.Queue
import           Data.Types

type RoomCache = AtomicLRU RoomID RoomState

data RoomPost = CreateRoom { roomNameParam :: Text, username :: Text }
              | JoinRoom   { roomIDParam :: RoomID, username :: Text }
              deriving (Eq, Ord, Read, Show)

createRoomParams :: [Text]
createRoomParams = ["room-name", "username"]

joinRoomParams :: [Text]
joinRoomParams = ["room-id", "username"]

userCookie :: UUID -> StrictText.Text
userCookie rid = "username-" <> (UUID.toText rid)

acceptedCookiesCookie :: StrictText.Text
acceptedCookiesCookie = "cookiesAccepted"

main :: IO ()
main = do
  let port = 80
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
    Sc.get "/room/:pRoomID" $
      Sc.file "room.html"
    -- TODO: setup some static middleware
    Sc.get "/static/client.js" $
      Sc.file "client.js"
    Sc.get "/static/style.css" $
      Sc.file "static/style.css"

websockApp :: RoomCache -> WS.ServerApp
websockApp cache pending = do
  putText "ws connected"
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  (msg :: Text) <- WS.receiveData conn
  WS.sendTextData conn $ ("initial> " :: Text) <> msg

  forever $ do
    WS.sendTextData conn $ ("loop data" :: Text)
    threadDelay $ 1 * 1000000

mkRoomPost :: [Sc.Param] -> Maybe RoomPost
mkRoomPost params = maybeJoin <|> maybeCreate
  where
  maybeJoin    = do
    roomIDP  <- param "room-id" params
    rID      <- textToUUID roomIDP
    username <- param "username" params
    pure $ JoinRoom (RoomID rID) username
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

getUsername :: RoomID -> Sc.ActionM (Maybe Text)
getUsername (RoomID rid) = (fmap . fmap) Text.fromStrict $ Sc.getCookie (userCookie rid)

uuidToText :: UUID -> Text
uuidToText = Text.fromStrict . UUID.toText

textToUUID :: Text -> Maybe UUID
textToUUID = UUID.fromText . Text.toStrict

roomIDToText :: RoomID -> Text
roomIDToText = uuidToText . unRoomID

param :: Text -> [Sc.Param] -> Maybe Text
param var = fmap snd . find (\(par, _) -> par == var)

lazyError :: Text -> a
lazyError t = error $ Text.toStrict t
