module KMeans where

import           Data.Vector (Vector (..))
import           KMeansCore
import           KMeansIO

-- -- Algorithm steps -----
-- - 1. Init random k-means
-- - 2. Iterate over and assign centroids to datapoints
-- - 3. Calculate new centroids for newly formed clusters

kmeansIO :: IO ()
kmeansIO = mainIO $ uncurry kMeans

kMeans :: Vector Point -> [Cluster] -> IO [Cluster]
kMeans points = loopKMeans
              $ nextStep points
nextStep :: Vector Point -> Int -> [Cluster] -> [Cluster]
nextStep points nmbCls clusters = mkNewCluster
                                $ assign nmbCls clusters points
