{-# language OverloadedStrings #-}

module Rasa.Websockets where

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import           Control.Monad.State (liftIO)
import           Control.Lens (use, (.=))
import           Data.Default
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as T
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

newtype StateRef = StateRef (Maybe (Concurrent.MVar State))
instance Default StateRef where
  def = StateRef Nothing
instance Show StateRef where
  show (StateRef (Just _)) = "ref"
  show (StateRef (Nothing)) = "no ref"

data Message = Message T.Text deriving (Typeable)
data Send = Send T.Text

nextId :: State -> ClientId
nextId = Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map fst

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId state
  return ((clientId, conn) :state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return $ withoutClient clientId state

--listen :: forall b a. WS.Connection -> (Message -> IO a) -> IO (b)
listen conn dispatch = Monad.forever $ do
  x <- WS.receiveData conn
  dispatch $ Message x

--wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef dispatch pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn dispatch)
    (disconnectClient clientId stateRef)

networkServer :: Action ()
networkServer = do
  stateRef <- liftIO $ Concurrent.newMVar ([] :: [Client])
  -- ext .= (StateRef $ Just stateRef)
  onInit $ asyncEventProvider (serverStart stateRef)

  onEveryTrigger_ (sendMsg stateRef)

--sendMsg :: WS.Connection -> Message -> Action ()
sendMsg stateRef (Send msg) = liftIO $
  Concurrent.readMVar stateRef
    >>= Monad.mapM_ (\(_, x) -> WS.sendTextData x msg)

--serverStart :: Concurrent.MVar State -> ((Message -> IO () ) -> IO () ) -> IO ()
serverStart state dispatch = do
    putStrLn "starting"
    Warp.run 3000 $ WS.websocketsOr
      WS.defaultConnectionOptions
      (wsApp state dispatch)
      httpApp
    putStrLn "server stopped"

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"
