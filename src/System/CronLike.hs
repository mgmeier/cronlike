

module System.CronLike
        ( cronlikeRegister
        , cronlikeUnregister
        , cronlikeIsRunning
        , module Types
        ) where

import  System.CronLike.Types as Types

import  Control.Monad
import  Control.Concurrent.MVar
import  Data.IORef
import  Data.Maybe                      (fromMaybe)
import  System.IO.Unsafe                (unsafePerformIO)



--- global mutables

-- empty -> not running; True -> running; False -> stop requested
gmutRunningState :: MVar Bool
gmutRunningState = unsafePerformIO newEmptyMVar
{-# NOINLINE gmutRunningState #-}

cronlikeRegister :: CronLikeJob -> IO ()
cronlikeRegister = undefined

cronlikeUnregister :: String -> IO ()
cronlikeUnregister = undefined

cronlikeIsRunning :: IO Bool
cronlikeIsRunning = fromMaybe False <$> tryReadMVar gmutRunningState

cronlikeStart :: IO ()
cronlikeStart = isEmptyMVar gmutRunningState >>= (`when` start)
  where
    start = do
        tryPutMVar gmutRunningState True
        putStrLn "starting cron"