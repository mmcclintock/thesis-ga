{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude
import qualified Data.Vector.Unboxed as U
import HsEA.GA

-- the environment for linear regression is a set of x,y points,
xvals = U.fromList [-1, 0, 3, 5]
yvals = U.fromList [-0.5, 0, 1.5, 2.5]
-- these are all points on the line y=0.5x so the perfect solution is m=0.5, y=0.0

-- here is where we define the fitness function given a vector of doubles
instance Genome Double where
  fitness v = 1000 / (1 + sse)  -- fitness goes between 0 and 1000
    where 
      m = v U.! 0  -- first parameter is the gradient
      c = v U.! 1  -- second parameter is the intercept
      sse = U.sum . U.map (^2) . U.zipWith (-) yvals . U.map (\x -> m*x + c) $ xvals  -- sum of squared error

-- this basically says: with an initial random population repeat
-- "evolve" n times and get the best individual of the final population
ga = do
  n <- asks numberGens
  finalPop <- foldl' (>>=) randomPopulation (replicate n evolve)
  getBest finalPop

-- this is the definition of evoltuion: calculate the fitness ->
-- choose the parents -> perform recombination -> perform mutation ->
-- ensure the best individual survives (elitism)
evolve pop = do
  fit <- calculateFitness pop
  off <- chooseParents fit pop >>= recombination >>= mutationReal
  elitism fit pop off
 
-- parameters for the GA.
conf rg = GAConfig { popSize = 100
                   , numberGens = 10000
                   , vecSize = 2
                   , mutationRate = 0.01
                   , recombinationRate = 0.5
                   , randomGenerator = rg
                   }

-- run the GA.
main = withSystemRandom (runReaderT ga . conf) >>= print
