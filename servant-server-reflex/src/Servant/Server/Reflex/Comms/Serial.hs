{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Servant.Server.Reflex.Comms.Serial (
    ReqRes
  , mkReqRes
  , reqRes
  , waitForRequest
  , enqueueResponse
  ) where

import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, readTMVar, putTMVar, takeTMVar)

data ReqRes a b =
  ReqRes (TMVar a) (TMVar b)

mkReqRes :: STM (ReqRes a b)
mkReqRes =
  ReqRes <$> newEmptyTMVar <*> newEmptyTMVar

enqueueRequest :: ReqRes a b -> a -> STM ()
enqueueRequest (ReqRes req _) a = do
  putTMVar req a

waitForResponse :: ReqRes a b -> STM b
waitForResponse (ReqRes _ res) = do
  takeTMVar res

-- for outside of the FRP network

reqRes :: ReqRes a b -> a -> IO b
reqRes rr a = do
  atomically $ enqueueRequest rr a
  atomically $ waitForResponse rr

-- for inside of the the FRP network

waitForRequest :: ReqRes a b -> STM a
waitForRequest (ReqRes req _) =
  readTMVar req

enqueueResponse :: ReqRes a b -> b -> STM ()
enqueueResponse (ReqRes req res) b = do
  putTMVar res b
  _ <- takeTMVar req
  return ()
