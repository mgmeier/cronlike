{-# LANGUAGE GADTs #-} 

module System.CronLike.Types where

import Data.Time.LocalTime  (TimeOfDay)
import Data.Time.Clock      (UTCTime, NominalDiffTime)


data CronLikeInterval
    = IntervEveryNMins  Int
    | IntervDailyAt     [TimeOfDay]
    | IntervScheduled   UTCTime (Maybe NominalDiffTime)
    deriving Show

data CronLikeJob idtoken where
    CronLikeJob :: (Eq idtoken, Show idtoken) =>
        { cjobId        :: idtoken
        , cjobInterval  :: CronLikeInterval
        , cjobAction    :: IO ()
        } -> CronLikeJob idtoken

instance Eq (CronLikeJob idtoken) where
    CronLikeJob a _ _ == CronLikeJob a' _ _ = a == a'

instance Show (CronLikeJob idtoken) where
  show (CronLikeJob name interv _) =
    "CronLikeJob " ++ show name ++ " @ " ++ show interv
