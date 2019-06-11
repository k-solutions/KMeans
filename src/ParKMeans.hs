module ParKMeans
    ( parKMeans
    , parKMeansIO
    ) where

import           Control.Parallel.Strategies (parTraversable, rseq, using)
import           Data.List                   (foldr1)
import           Data.Vector                 (Vector (..))
import qualified Data.Vector                 as V
import           KMeansCore
import           KMeansIO

---- API ----

parKMeansIO :: Int -> IO ()
parKMeansIO n = mainIO
            $ uncurry (parKMeans n)

parKMeans :: Int -> Vector Point -> [Cluster] -> IO [Cluster]
parKMeans nmbCks points clusters =
    let pChunks   = split nmbCks points
        nxtStepFn = parStep pChunks
    in loopKMeans nxtStepFn clusters

-- ---- Helpers ----

chunks :: Int -> Vector a -> Vector (Vector a)
chunks n xs
  | V.null xs = V.empty
  | otherwise = as `V.cons` chunks n bs
  where (as, bs) = V.splitAt n xs

split :: Int -> Vector a -> Vector (Vector a)
split n xs  = chunks (V.length xs `quot` n) xs

parStep :: Vector (Vector Point) -> Int -> [Cluster] -> [Cluster]
parStep pointss nmbCls clusters = mkNewCluster
                                $ foldr1 combine
                                    (V.map (assign nmbCls clusters) pointss `using` parTraversable rseq)
