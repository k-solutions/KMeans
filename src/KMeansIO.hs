{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module KMeansIO
    ( Config (..)
    , App (..)
    , runApp
    , runAction
    , assign
    , loopKMeans
    , timeIt
    ) where

import           KMeansCore

import           Control.Concurrent   (runInUnboundThread)
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Binary          (Binary (..), decodeFile)
import           Data.Function        (on)
import           Data.List            (foldl', maximumBy)
import           Data.Text.Lazy       (Text (..))
import qualified Data.Text.Lazy       as Tx (pack, unpack)
import           Data.Time.Clock      (diffUTCTime, getCurrentTime)
import           Data.Vector          (Vector (..))
import qualified Data.Vector          as V
import qualified Data.Vector.Mutable  as MV (read, replicate, write)
import           System.Mem
import           Text.Printf

-- -- Algorithm steps -----
-- - 1. Init random k-means
-- - 2. Iterate over and assign centroids to datapoints
-- - 3. Calculate new centroids for newly formed clusters

data Config = Config
            { cfgPointFile   :: FilePath
            , cfgClusterFile :: FilePath
            , cfgMaxSize     :: Int

            } deriving (Eq, Show)

type Log      = Text
type AppT     = ReaderT Config (WriterT Log IO)
newtype App a = App { unApp :: AppT a } deriving (Applicative, Functor, Monad, MonadReader Config, MonadWriter Log, MonadIO)

-- ---- API ----
runApp :: App a -> Config -> IO ()
runApp app cfg  =
    getLogW >>= putStr . getLog
  where
    getLog  = Tx.unpack . snd
    getLogW = runWriterT $ getR app cfg
    getR    = runReaderT . unApp

runAction :: (Binary b, Read a, Show a) => ((Vector b, a) -> App a) -> App ()
runAction action = do
  cfg         <- ask
  inp <- liftIO $ do
    points    <- V.fromList <$> decodeFile (cfgPointFile cfg) -- "points.bin"
    clusters  <- read <$> readFile (cfgClusterFile cfg)       -- "clusters"
    return (points, clusters)
  res <- action inp
  tell . Tx.pack . show $ res -- (points, clusters)

initKmeans :: IO ()
initKmeans = do
   params <- getParams
   genPoints >> genClusters
   writeFile "params" $ show params
  where
    genPoints = undefined
    genClusters = undefined

-- ---- IO Helpers ----

timeIt :: Show a => IO a -> IO ()
timeIt action = do
    performGC
    t0  <- getCurrentTime
    res <- action
    t1  <- getCurrentTime

    printf "------------ Total time: %.2f -----------\n" (realToFrac (diffUTCTime t1 t0) :: Double)
    print res


-- | A function should take points and clusters and return new clusters
type NextStep = Int -> [Cluster] -> [Cluster]

--  | Main loop for KMeans algorith
loopKMeans :: NextStep -> [Cluster] -> App [Cluster]
loopKMeans nextStepFun clusters =
  let
    nmbCls = length clusters
    loop :: Int -> [Cluster] -> App [Cluster]
    loop n clusters = do
      maxSize <- asks cfgMaxSize
      if n > maxSize
        then do
          tell "we run out of numbers"
          return clusters
        else do
          tell . Tx.pack . unlines $ ("interation " ++ show n): map show clusters
          let clusters' = nextStepFun nmbCls clusters  -- points
          if clusters' == clusters
            then return clusters
            else loop (n + 1) clusters'
  in loop 0 clusters

-- ---- Plain Helpers ----

assign :: Int -> [Cluster] -> Vector Point -> Vector PointSum
assign nmbCls clusters points = V.create $ do
  vec <- MV.replicate nmbCls emptyPointSum
  let  addPoint p = do
        let cId = clId $ nearCluster p
        ps <- MV.read vec cId
        MV.write vec cId $! addToPointSum ps p
  V.mapM_ addPoint points
  return vec
  where
    nearCluster p = fst
                  $ maximumBy (compare `on` snd)
                  [(c, sqDistance p $ clCent c) | c <- clusters]


getParams :: IO [Point]
getParams = undefined
