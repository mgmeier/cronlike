{-# LANGUAGE LambdaCase, BangPatterns #-}

module System.CronLike
        ( cronlikeRegister
        , cronlikeUnregister
        , cronlikeList
        , module Types
        ) where

import  System.CronLike.Types           as Types

import  Data.Time.Clock
import  Data.Time.LocalTime             (timeOfDayToTime)
import  Control.Concurrent.Async        (race)
import  Control.DeepSeq                 (deepseq)
import  Control.Concurrent              (ThreadId, threadDelay, forkIO)
import  Control.Monad
import  Control.Concurrent.MVar
import  Data.Maybe                      (mapMaybe, fromJust)
import  Data.List                       (minimumBy)
import  Data.Ord                        (comparing)
import  System.IO.Unsafe                (unsafePerformIO)


type Command a = Either (a -> Bool) (CronLikeJob a)


--- global mutables

-- the only producer for gmutJobs is scheduler
gmutJobs :: MVar [(UTCTime, CronLikeJob a)]
gmutJobs = unsafePerformIO $ newMVar []
{-# NOINLINE gmutJobs #-}

-- the only producers for gmutControl are cronlikeRegister / Unregister
gmutControl :: MVar (Command a)
gmutControl = unsafePerformIO newEmptyMVar
{-# NOINLINE gmutControl #-}


-- worker thread

{-# NOINLINE scheduler #-}
scheduler :: ThreadId
scheduler = unsafePerformIO $ forkIO $ forever $
    readJobsCleaningUp >>= \case
        ([], _) -> event >>= control
        jsNow   -> race (nextJob jsNow) event >>= either runJob control
  where
    event = takeMVar gmutControl 

    runJob :: CronLikeJob a -> IO ()
    runJob job = do
        now <- getCurrentTime
        modifyMVar_ gmutJobs $
            return . map (\j@(_, job') -> if job == job' then (now, job') else j)
        void $ forkIO $ cjobAction job

    control :: Command a -> IO ()
    control = modifyMVar_ gmutJobs . either rmJobs addJob
      where
        rmJobs cond = return . filter (not . cond . cjobId . snd)
        addJob job jobs = do
            now <- getCurrentTime
            return $ (now, job) : filter ((/= job) . snd) jobs

    readJobsCleaningUp = modifyMVar gmutJobs $ \js -> do
        now <- getCurrentTime
        let js' = mapMaybe (filterScheduled now) js
        return (js', (js', now))
      where
        filterScheduled now (lastRun, j) =
            (\i' -> (lastRun, j {cjobInterval = i'})) <$> advance now (cjobInterval j)

    nextJob (js, now) = threadDelay (1000 * 1000 * round wait) >> return job
      where
        delayPerJob = [(fromNow now lastRun (cjobInterval j), j) | (lastRun, j) <- js]
        (wait, job) = minimumBy (comparing fst) delayPerJob



-- helper functions 

fromNow :: UTCTime -> UTCTime -> CronLikeInterval -> NominalDiffTime
fromNow now@(UTCTime day tod) lastRun = \case
    IntervScheduled t _ -> t `diffUTCTime` now
    IntervEveryNMins n  -> fromIntegral (60 * n) `addUTCTime` lastRun `diffUTCTime` now
    IntervDailyAt is    -> minimum [next `diffUTCTime` now | next <- map (nextDaily . timeOfDayToTime) is]
  where
    nextDaily target    = UTCTime (if target < tod then succ day else day) target

advance :: UTCTime -> CronLikeInterval -> Maybe CronLikeInterval
advance now = \case
    i@(IntervScheduled t Nothing)   -> if t < now then Nothing else Just i
    i@(IntervScheduled t d)         -> Just $ if t < now then IntervScheduled (go (fromJust d) t) d else i
    i                               -> Just i
  where
    go !diff !t = let t' = diff `addUTCTime` t in if t' < now then go diff t' else t'



-- API

-- the scheduler thread starts as soon as the first job is registered
cronlikeRegister :: CronLikeJob a -> IO ()
cronlikeRegister job =
    scheduler `deepseq` putMVar gmutControl (Right job)

cronlikeUnregister :: (a -> Bool) -> IO ()
cronlikeUnregister = putMVar gmutControl . Left

cronlikeList :: IO [CronLikeJob a]
cronlikeList = map snd <$> readMVar gmutJobs

