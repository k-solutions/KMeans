module Main where

import           Control.Concurrent (runInUnboundThread)
import           KMeans
import           KMeansIO           (Config (..))
import           ParKMeans
import           System.Environment

main :: IO ()
main = runInUnboundThread $ do
  args <- getArgs
  case args of
    [ "sec"    ] -> runKMeans appCfg
    [ "par", n ] -> runParKMeans (read n) appCfg
    _            -> error "args"
  where
    appCfg = Config
           { cfgPointFile   = "data/points.bin"
           , cfgClusterFile = "data/clusters"
           , cfgMaxSize     = 80
           }
