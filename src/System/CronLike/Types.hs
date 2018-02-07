
module System.CronLike.Types where

import Data.Time.LocalTime  (TimeOfDay)
import Data.Time.Clock      (UTCTime, NominalDiffTime)


data CronLikeIntervall
    = IntervEveryNMins  Int
    | IntervDailyAt     [TimeOfDay]
    | IntervScheduled   UTCTime (Maybe NominalDiffTime)
    deriving Show

data CronLikeJob = CronLikeJob
    { cjobName      :: String
    , cjobInterval  :: CronLikeIntervall
    , cjobAction    :: IO ()
    }

instance Show CronLikeJob where
    show c = "CronLikeJob " ++ cjobName c ++ " @ " ++ show (cjobInterval c)
