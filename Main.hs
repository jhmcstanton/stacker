import           Protolude                            hiding (Text)

import           Data.Cache.LRU.IO                    (AtomicLRU)
import qualified Data.Cache.LRU.IO                    as Cache
import           Data.UUID                            (UUID)
import qualified Data.UUID                            as UUID
import qualified Data.UUID.V4                         as UUID
import           Data.Sequence                        (Seq)
import qualified Data.Sequence                        as Seq
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as Text
import qualified Web.Scotty                           as Sc
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

data RoomPost = CreateRoom
              | JoinRoom   { roomIDParam :: RoomID }
              deriving (Eq, Ord, Read, Show)

joinRoomParams :: [Text]
joinRoomParams = ["room-id"]

main :: IO ()
main = do
  let port = 80
  let settings = Warp.setPort port Warp.defaultSettings
  cache <- Cache.newAtomicLRU Nothing
  sapp <- scottyApp cache
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
        Nothing -> Sc.status Sc.badRequest400
        Just CreateRoom -> createRoom cache
        Just jr         -> do
          let rid = unRoomID . roomIDParam $ jr
          Sc.redirect $ "/room/" <> uuidToText rid
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
mkRoomPost params =
  case maybeJoin of
    Nothing -> Just CreateRoom
    Just _  -> maybeJoin
  where
  joinParams = filter (\(name, _) -> name `elem` joinRoomParams) params
  maybeJoin  = do
    (_, roomIDP) <- find (\(name, _) -> name == "room-id") joinParams
    rID          <- textToUUID roomIDP
    pure . JoinRoom . RoomID $ rID

createRoom :: RoomCache -> Sc.ActionM ()
createRoom cache = do
  rstate <- Sc.liftAndCatchIO $ do
    rID   <- fmap RoomID UUID.nextRandom
    let rstate = newRoom rID
    Cache.insert rID rstate cache
    putLText $ "Created room with ID [" <> roomIDToText rID <> "]"
    pure rstate

  let rid   = unRoomID $ roomID rstate
  Sc.redirect $ "/room/" <> uuidToText rid

uuidToText :: UUID -> Text
uuidToText = Text.fromStrict . UUID.toText

textToUUID :: Text -> Maybe UUID
textToUUID = UUID.fromText . Text.toStrict

roomIDToText :: RoomID -> Text
roomIDToText = uuidToText . unRoomID
