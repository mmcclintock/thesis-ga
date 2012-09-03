{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import BasicPrelude
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import HsEA.GA

-- the environment
xvals :: UVector Double
xvals = U.fromList [-1, 0, 3, 5]

yvals :: UVector Double
yvals = U.fromList [-0.5, 0, 1.5, 2.5]


-- define the GA genotype as a unboxed vector of doubles 
newtype DoubleVec = DV { toDV :: UVector Double } deriving Show
instance GAGenotype DoubleVec Double where

  toVec = toDV
  fromVec = DV

  fitness dv = 1000 / (1 + sse )
    where 
      Line m c = decode dv
      ys = U.map (\x -> m*x + c) xvals
      err = U.zipWith (-) ys yvals
      sse = U.sum $ U.map (^(2 :: Int)) err


-- GA has no knowledge of the phenotype so it's purely problem
-- specific
data Line = Line !Double !Double deriving Show

decode :: DoubleVec -> Line
decode dv = Line m c
  where
    v = toVec dv
    m = v U.! 0
    c = v U.! 1

-- Configuration
conf :: GAConfig
conf = GAConfig { popSize = 200
                , numberGens = 1000
                , vecSize = 2
                , mutationRate = 0.2
                , recombinationRate = 0.6
                }

ga :: GAStack (DoubleVec, Fitness)
ga = do
  initPop <- randomPopulation
  n <- asks numberGens
  finalPop <- evolve n initPop
  getBest finalPop

evolve :: Int -> Vector DoubleVec -> GAStack (Vector DoubleVec)
evolve 0 v = return v
evolve n pop = do
  fit <- calculateFitness pop
  off <- chooseParents fit pop >>= recombination >>= mutationReal
  new <- elitism fit pop off
  evolve (n-1) new
 
main :: IO ()
main = do
  putStrLn "Starting GA!"

  gen <- newStdGen
  putStrLn $ "Generated random seed: " ++ show gen

  print (runGAStack ga conf gen)
