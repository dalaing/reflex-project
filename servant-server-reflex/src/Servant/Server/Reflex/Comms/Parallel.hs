{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Servant.Server.Reflex.Comms.Parallel (
    Ticket
  , ReqRes
  , mkReqRes
  , reqRes
  , waitForRequest
  , enqueueResponse
  ) where

import Control.Monad.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, writeTBQueue, readTBQueue)
import qualified Control.Concurrent.STM.Map as SM
import Data.Hashable (Hashable(..))

newtype Ticket = Ticket Int
  deriving (Eq, Ord, Show, Hashable)

data ReqRes a b =
  ReqRes (TVar Ticket) (TBQueue (Ticket, a)) (SM.Map Ticket b)

mkReqRes :: Int -> STM (ReqRes a b)
mkReqRes size =
  ReqRes <$> newTVar (Ticket 0) <*> newTBQueue size <*> SM.empty

getTicket :: TVar Ticket -> STM Ticket
getTicket tv = do
  Ticket t <- readTVar tv
  writeTVar tv $ Ticket (succ t)
  return $ Ticket t

enqueueRequest :: ReqRes a b -> a -> STM Ticket
enqueueRequest (ReqRes tv req _) a = do
  t <- getTicket tv
  writeTBQueue req (t, a)
  return t

waitForResponse :: ReqRes a b -> Ticket -> STM b
waitForResponse (ReqRes _ _ res) t = do
  v <- SM.lookup t res
  case v of
    Just x ->
      return x
    Nothing -> do
      retry

-- for outside of the FRP network
reqRes :: ReqRes a b -> a -> IO b
reqRes rr a = do
  t <- atomically $ enqueueRequest rr a
  atomically $ waitForResponse rr t

-- for inside of the FRP network
waitForRequest :: ReqRes a b -> STM (Ticket, a)
waitForRequest (ReqRes _ req _) =
  readTBQueue req

enqueueResponse :: ReqRes a b -> Ticket -> b -> STM ()
enqueueResponse (ReqRes _ _ res) t b =
  SM.insert t b res
