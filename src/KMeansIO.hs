module KMeansIO
    ( mainIO
    ) where

import           KMeansCore

import           Control.Concurrent (runInUnboundThread)
import           Data.Binary        (Binary (..), decodeFile)
import           Data.Time.Clock    (diffUTCTime, getCurrentTime)
import           Data.Vector        (Vector (..))
import qualified Data.Vector        as V (fromList)
import           System.Mem
import           Text.Printf

-- -- Algorithm steps -----
-- - 1. Init random k-means
-- - 2. Iterate over and assign centroids to datapoints
-- - 3. Calculate new centroids for newly formed clusters

mainIO :: (Binary b, Read a, Show a) => ((Vector b, a) -> IO a) -> IO ()
mainIO action = do
  points    <- V.fromList <$> decodeFile "points.bin"
  clusters  <- read <$> readFile "clusters"
  timeIt $ action (points, clusters)

timeIt :: Show a => IO a -> IO ()
timeIt action = do
    performGC
    t0  <- getCurrentTime
    res <- action
    t1  <- getCurrentTime

    printf "------------ Total time: %.2f -----------\n" (realToFrac (diffUTCTime t1 t0) :: Double)
    print res

initKmeans :: IO ()
initKmeans = do
   params <- getParams
   genPoints >> genClusters
   writeFile "params" $ show params
  where
    genPoints = undefined
    genClusters = undefined

-- ---- IO Helpers ----

getParams :: IO [Point]
getParams = undefined
