module KMeans where

import           KMeansCore
import           KMeansIO

--import           Control.Concurrent (runInUnboundThread)
--import           Data.Binary        (decodeFile)
--import           Data.Time.Clock    (diffUTCTime, getCurrentTime)
--import           System.Mem
--import           Text.Printf

-- -- Algorithm steps -----
-- - 1. Init random k-means
-- - 2. Iterate over and assign centroids to datapoints
-- - 3. Calculate new centroids for newly formed clusters

kmeansIO :: IO ()
kmeansIO = mainIO $ uncurry kMeans

kMeans :: [Point] -> [Cluster] -> IO [Cluster]
kMeans points = loopKMeans
              $ nextStep points
--   let
--     loop  n clusters | n > maxSize = do
--       putStrLn "we run out of numbers"
--       return clusters
--     loop n clusters = do
--       putStrLn $ "interation " ++ show n
--       putStr $ unlines $ map show clusters
--       let clusters' = nextStep nmbCls clusters points
--       if clusters' == clusters
--         then return clusters
--         else loop (n+1) clusters'
--   in loop 0 clusters

nextStep :: [Point] -> Int -> [Cluster] -> [Cluster]
nextStep points nmbCls clusters = mkNewCluster
                                $ assign nmbCls clusters points
