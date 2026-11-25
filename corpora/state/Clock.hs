-- One host clock for periodic application workers. Storage commits independently.
module Clock (Clock, start, stop, subscribe) where
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import GHC.Clock

data Clock = Clock (TChan Integer) (Async ())
start :: IO Clock
start = do
 ticks <- newBroadcastTChanIO
 origin <- getMonotonicTimeNSec
 worker <- async $ let loop due count = do
                        now <- getMonotonicTimeNSec
                        when (now < due) (threadDelay (fromIntegral ((due-now) `div` 1000)))
                        atomically (writeTChan ticks count)
                        next <- getMonotonicTimeNSec
                        let elapsed = max 1 ((next-due) `div` 500000000+1)
                        loop (due+elapsed*500000000) (count+fromIntegral elapsed)
                   in loop (origin+500000000) 1
 pure (Clock ticks worker)
stop :: Clock -> IO ()
stop (Clock _ worker) = cancel worker
subscribe :: Clock -> (Integer -> IO ()) -> IO (Async ())
subscribe (Clock ticks _) action = do
 channel <- atomically (dupTChan ticks)
 async $ forever $ do
  tick <- atomically $ do
   first <- readTChan channel
   let latest n = tryReadTChan channel >>= maybe (pure n) latest
   latest first
  action tick
