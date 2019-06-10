module Lib
    ( someFunc
    ) where

import           Control.Monad             (replicateM)
import qualified Control.Monad.Trans       as MTL
import           Control.Monad.Trans.State (State (..), evalState, get, put,
                                            runStateT)
import           Data.Random
import           Data.RVar
import           System.Random             (StdGen (..), getStdGen)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

rWalkState :: RVarT (State Double) Double
rWalkState = do
    prev   <- MTL.lift get
    change <- rvarT StdNormal
    let new = prev + change
    MTL.lift $ put new
    return new

rWalk :: Int -> Double -> StdGen -> ([Double], StdGen)
rWalk count start gen = flip evalState  start
                      . flip runStateT  gen
                      . sampleRVarTWith MTL.lift
                      $ replicateM count rWalkState
