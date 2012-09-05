{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import BasicPrelude
import qualified Data.Vector.Unboxed as U
import HsEA.GA

-- the environment
xvals :: UVector Double
xvals = U.fromList [-1, 0, 3, 5]

yvals :: UVector Double
yvals = U.fromList [-0.5, 0, 1.5, 2.5]


instance Genome Double where

  fitness v = 1000 / (1 + sse)
    where 
      m = v U.! 0
      c = v U.! 1
      sse = U.sum . U.map (^2) . U.zipWith (-) yvals . U.map (\x -> m*x + c) $ xvals


ga :: GAStack (UVector Double, Fitness)
ga = do
  n <- asks numberGens
  initPop <- randomPopulation
  finalPop <- foldl (>>=) (return initPop) (replicate n evolve)
  getBest finalPop

evolve :: Vector (UVector Double) -> GAStack (Vector (UVector Double))
evolve pop = do
  fit <- calculateFitness pop
  off <- chooseParents fit pop >>= recombination >>= mutationReal
  elitism fit pop off
 
main :: IO ()
main = do

  let conf = GAConfig 100 10000 2 0.1 0.6
  withSystemRandom (runReaderT ga . conf) >>= print
