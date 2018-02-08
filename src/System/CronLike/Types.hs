{-# LANGUAGE GADTs #-} 
module System.CronLike.Types where

import Data.Time.LocalTime  (TimeOfDay)
import Data.Time.Clock      (UTCTime, NominalDiffTime)


data CronLikeIntervall
    = IntervEveryNMins  Int
    | IntervDailyAt     [TimeOfDay]
    | IntervScheduled   UTCTime (Maybe NominalDiffTime)
    deriving Show


data CronLikeJob idtoken where
    CronLikeJob :: (Eq idtoken, Show idtoken) =>
        { cjobId        :: idtoken
        , cjobInterval  :: CronLikeIntervall
        , cjobAction    :: IO ()
        } -> CronLikeJob idtoken

instance Show (CronLikeJob idtoken) where
  show (CronLikeJob name interv _) =
    "CronLikeJob " ++ show name ++ " @ " ++ show interv
