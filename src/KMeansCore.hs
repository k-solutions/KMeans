module KMeansCore
  ( Point (..)
  , PointSum (..)
  , Cluster (..)
  , mkNewCluster
  , emptyPointSum
  , combine
  , sqDistance
  , addToPointSum
  ) where

import           Control.DeepSeq
import           Data.Binary
import qualified Data.ByteString.Char8 as BC
import           Data.List             (foldl')
import           Data.Vector           (Vector (..))
import qualified Data.Vector           as V

---- Algorithm steps
--- 1. Init random k-means
--- 2. Iterate over and assign centroids to datapoints
--- 3. Calculate new centroids for newly formed clusters

-- -- POINTS -- --

data Point = Point !Double !Double
           deriving (Eq, Read, Show)

instance NFData Point where
    rnf (Point x y) = ()

instance Binary Point where
    put (Point x y) = put x >> put y
    get             = Point <$> get <*> get

zeroPoint :: Point
zeroPoint = Point 0 0

sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = (x1 - x2)^2 + (y1 - y2)^2

-- - IO Points ----

readPoints :: FilePath -> IO [Point]
readPoints file = do
    cnt <- BC.readFile file
    let ws = map BC.words $ BC.lines cnt
    pure [Point (read $ BC.unpack x) (read $ BC.unpack y) | (x:y:_) <- ws]

-- ---- PointSums ----

data PointSum = PointSum !Int !Double !Double  --- temporary type to help with calculation

instance NFData PointSum where
  rnf (PointSum n x y) = ()

addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum ns xs ys) (Point x y) = PointSum (ns + 1) (xs + x) (ys + y)

addPointSums :: PointSum -> PointSum -> PointSum
addPointSums (PointSum ns1 xs1 ys1) (PointSum ns2 xs2 ys2) = PointSum (ns1 + ns2) (xs1 + xs2) (ys1 + ys2)

combine :: Vector PointSum -> Vector PointSum -> Vector PointSum
combine = V.zipWith addPointSums

-- ---- PointSum Helpers ----

emptyPointSum :: PointSum
emptyPointSum = PointSum 0 0 0

-- ---- Clusters ----

data Cluster = Cluster
             { clId   :: !Int
             , clCent :: !Point
             } deriving (Eq, Read, Show)

instance NFData Cluster where
     rnf Cluster {clId = cid, clCent = cCen} = ()

mkCluster :: Int -> [Point] -> Cluster
mkCluster cid points = Cluster { clId   = cid
                               , clCent = Point (a / cSize) (b / cSize)
                               }
  where
    cSize     = fromIntegral $ length points
    Point a b = foldl' addPoints zeroPoint points

    addPoints :: Point -> Point -> Point
    addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster cId (PointSum nSize xs ys) =
    Cluster { clId   = cId
            , clCent = Point (xs / fromIntegral nSize) (ys / fromIntegral nSize)
            }

mkNewCluster :: Vector PointSum -> [Cluster]
mkNewCluster vPs =  [ pointSumToCluster i ps
                    | (i, ps@(PointSum nSize _ _)) <- zip [0..] (V.toList vPs)
                    , nSize > 0
                    ] --- we filter out empty cluster as they would mess up with counts
