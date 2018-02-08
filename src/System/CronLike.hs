{-# LANGUAGE LambdaCase #-}

module System.CronLike
        ( cronlikeRegister
        , cronlikeUnregister
        , cronlikeList
        , module Types
        ) where

import  System.CronLike.Types as Types

import  Control.Concurrent.Async        (race)
import  Control.DeepSeq                 (deepseq)
import  Control.Concurrent              (ThreadId, threadDelay, forkIO)
import  Control.Monad
import  Control.Concurrent.MVar
import  System.IO.Unsafe                (unsafePerformIO)


type Command a = Either (a -> Bool) (CronLikeJob a)

--- global mutables

-- the only producer for gmutJobs is scheduler
gmutJobs :: MVar [CronLikeJob a]
gmutJobs = unsafePerformIO $ newMVar []
{-# NOINLINE gmutJobs #-}

-- the only producers for gmutControl are cronlikeRegister / Unregister
gmutControl :: MVar (Command a)
gmutControl = unsafePerformIO newEmptyMVar
{-# NOINLINE gmutControl #-}


-- worker thread

scheduler :: ThreadId
scheduler = unsafePerformIO $ forkIO $ forever $
    readMVar gmutJobs >>= \case
        [] -> event >>= control
        js -> race (nextJob js) event >>= either runJob control
  where
    event = takeMVar gmutControl 

    runJob :: CronLikeJob a -> IO ()
    runJob = void . forkIO . cjobAction

    control :: Command a -> IO ()
    control command = modifyMVar_ gmutJobs (return . modify)
      where
        -- TODO filter existing cJobId when adding
        modify = either (\cond -> filter (not . cond . cjobId)) (:) command

    nextJob js = do 
        -- handle filtering out scheduled events in the past
        threadDelay (findShortestDelay js)
        return $ head js

    findShortestDelay _ = 
        60 * 1000 * 1000
  


-- API

-- the scheduler thread starts as soon as the first job is registered
cronlikeRegister :: CronLikeJob a -> IO ()
cronlikeRegister job =
    scheduler `deepseq` putMVar gmutControl (Right job)

cronlikeUnregister :: (a -> Bool) -> IO ()
cronlikeUnregister = putMVar gmutControl . Left

cronlikeList :: IO [CronLikeJob a]
cronlikeList = readMVar gmutJobs

