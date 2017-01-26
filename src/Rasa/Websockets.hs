{-# language OverloadedStrings, Rank2Types #-}

module Rasa.Websockets (ClientId, Message, Send (..), websockets, ClientAction(..))
where

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import           Control.Monad.State (liftIO)
import qualified Data.Text                      as T
import qualified Data.ByteString.Lazy           as BL
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe
import Data.Typeable (Typeable)

import Rasa.Ext

type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

data Message = Message T.Text deriving (Typeable)
data Send = Send [ClientId] BL.ByteString | SendAll BL.ByteString
data ClientAction = NewClient ClientId | LostClient ClientId

nextId :: State -> ClientId
nextId = maybe 0 ((+) 1) . Safe.maximumMay . map fst

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId state
  return ((clientId, conn) :state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return $ withoutClient clientId state

listen :: WS.Connection -> Dispatcher -> IO ()
listen conn dispatch = Monad.forever $ do
  x <- (WS.receiveData conn) :: IO (T.Text)
  dispatch $ Message x

wsApp :: Concurrent.MVar State -> Dispatcher -> WS.ServerApp
wsApp stateRef dispatch pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (dispatch (NewClient clientId)  >> listen conn dispatch)
    (dispatch (LostClient clientId) >> disconnectClient clientId stateRef)

websockets :: Int -> Action ()
websockets port = do
  stateRef <- liftIO $ Concurrent.newMVar ([] :: [Client])
  onInit $ asyncEventProvider (serverStart stateRef port)

  onEveryTrigger_ (sendMsg stateRef)

sendMsg :: Concurrent.MVar State -> Send -> Action ()
sendMsg stateRef (SendAll msg) = liftIO $
  Concurrent.readMVar stateRef
    >>= Monad.mapM_ (\(_, x) -> WS.sendTextData x msg)
sendMsg stateRef (Send clientIds msg) = liftIO $ do
  state <- Concurrent.readMVar stateRef
  Monad.mapM_ (\x -> WS.sendTextData (snd x) msg) (toSendto state)
  where
    toSendto = filter (\x -> (fst x) `elem` clientIds)

serverStart :: Concurrent.MVar State -> Int -> Dispatcher -> IO ()
serverStart state port dispatch = do
    Warp.run port $ WS.websocketsOr
      WS.defaultConnectionOptions
      (wsApp state dispatch)
      httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"
