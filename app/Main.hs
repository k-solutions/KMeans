module Main where

import           Control.Concurrent (runInUnboundThread)
import           KMeans
import           ParKMeans
import           System.Environment
--- import qualified Data.Vector as V (fromList)

main :: IO ()
main = runInUnboundThread $ do
  args <- getArgs
  case args of
    [ "sec"    ] -> kmeansIO
    [ "par", n ] -> parKMeansIO (read n)
    _            -> error "args"
