{-# LANGUAGE LambdaCase #-}

module System.CronLike
        ( cronlikeRegister
        , cronlikeUnregister
        , cronlikeIsRunning
        , cronlikeList
        , cronlikeStop
        , cronlikeStart
        , module Types
        ) where

import  System.CronLike.Types as Types

import  Control.Concurrent.Async        (race)
import  Control.Concurrent              (threadDelay, forkIO)
import  Control.Monad
import  Control.Concurrent.MVar
import  Data.Maybe
import  System.IO.Unsafe                (unsafePerformIO)


type Command = Either String CronLikeJob

--- global mutables

-- full -> stopped; empty -> running
gmutRunning :: MVar ()
gmutRunning = unsafePerformIO $ newMVar ()
{-# NOINLINE gmutRunning #-}

gmutJobs :: MVar [CronLikeJob]
gmutJobs = unsafePerformIO $ newMVar []
{-# NOINLINE gmutJobs #-}

gmutControl :: MVar Command
gmutControl = unsafePerformIO newEmptyMVar
{-# NOINLINE gmutControl #-}




scheduler :: IO ()
scheduler =
    readMVar gmutJobs >>= \case
        [] -> event >>= control
        js -> race (nextJob js) event >>= react
  where
    react = either (\j -> scheduler) control
    event = race (takeMVar gmutRunning) (takeMVar gmutControl)
   
    control :: Either () Command -> IO ()
    control Left{} = cronlikeStop
    control (Right command) = do
        modifyMVar_ gmutJobs (return . modify)
        scheduler
      where
        modify = either (\n -> filter ((/= n) . cjobName)) (:) command

    nextJob js = do 
        -- handle filtering out scheduled events in the past
        threadDelay (findShortestDelay js)
        return $ head js
    
    findShortestDelay _ = 
        60 * 1000 * 1000
  


-- API

cronlikeRegister :: CronLikeJob -> IO ()
cronlikeRegister = putMVar gmutControl . Right 

cronlikeUnregister :: String -> IO ()
cronlikeUnregister = putMVar gmutControl . Left

cronlikeList :: IO [CronLikeJob]
cronlikeList = readMVar gmutJobs

cronlikeIsRunning :: IO Bool
cronlikeIsRunning = isEmptyMVar gmutRunning

cronlikeStop :: IO ()
cronlikeStop = void $ tryPutMVar gmutRunning ()

cronlikeStart :: IO ()
cronlikeStart =
    tryTakeMVar gmutRunning >>= \res ->
        when (isJust res) (void $ forkIO scheduler)
