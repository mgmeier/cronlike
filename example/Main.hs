
import System.CronLike

import Control.Concurrent (threadDelay)
import Control.Monad      (forever)
import Data.IORef

main :: IO ()
main = do
    putStrLn "starting cronjobs, wait 10s for job list..."
    newCounterJob "1" 2 >>= cronlikeRegister
    sleep
    newCounterJob "2" 1 >>= cronlikeRegister
    sleep
    
    cronlikeList >>= mapM_ print
    putStrLn $ "now putting main thread to sleep..."
    forever sleep
  where
    sleep = threadDelay (5 * 1000 * 1000)

    -- a job that counts up the times it has been called, unregistering itself after 5 calls
    newCounterJob name mins = do
        count <- newIORef 0
        return $ CronLikeJob jobId (IntervEveryNMins mins) (action count) 
      where
        jobId = "counter_" ++ name
        action count = do
            t <- readIORef count
            putStrLn $ name ++ " has been called " ++ show t ++ " times before"
            if t >= 4
                then putStrLn (name ++ " unregistering...") >> cronlikeUnregister (== jobId)
                else writeIORef count (t+1)
