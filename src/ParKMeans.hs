module ParKMeans
    ( parKMeans
    , parKMeansIO
    ) where

import           Control.Parallel.Strategies (parList, rseq, using)
import           Data.List                   (foldr1)
import           Data.Vector                 (Vector (..))

import           KMeansCore
import           KMeansIO

---- API ----

parKMeansIO :: Int -> IO ()
parKMeansIO n = mainIO
            $ uncurry (parKMeans n)

parKMeans :: Int -> [Point] -> [Cluster] -> IO [Cluster]
parKMeans nmbCks points clusters =
    let pChunks   = chunks nmbCks points
        nxtStepFn = parStep pChunks
    in loopKMeans nxtStepFn clusters

-- ---- Helpers ----

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = as : chunks n bs
  where (as, bs) = splitAt n xs

split :: Int -> [a] -> [[a]]
split n xs = chunks (length xs `quot` n) xs

parStep :: [[Point]] -> Int -> [Cluster] -> [Cluster]
parStep pointss nmbCls clusters = mkNewCluster
                                $ foldr1 combine
                                    (map (assign nmbCls clusters) pointss `using` parList rseq)
