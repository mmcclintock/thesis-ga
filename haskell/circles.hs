{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import BasicPrelude
import qualified Data.Vector.Unboxed as U
import HsEA.GA


-- fist lets think about the phenotype, the best way to represent a
-- solution.

type X = Double
type Z = Double
type Radius = Double
type Density = Double

data Ore = Ore !X !Z !Radius !Density

-- this is how the fiels is calculated
gravity :: Ore -> (X, Z) -> (X, Z)
gravity (Ore xo zo r p) (x, z) = (k*(xo - x), k*(zo - z))
  where 
    g = 6.67384e-11
    m = p*4*pi*r^3/3
    s = sqrt $ (x - xo)^2 + (z - zo)^2 
    k = g*m/s^3

-- now lets define the enviroment
-- we have gravity field readings at x,z locations
-- lets generate readings that a sphere located at x=500 z=-1000 r=250
-- would create

solution = Ore 500 (-1000) 250 2500

locations :: UVector (X, Z)
locations = U.zip (U.enumFromN 0 100) (U.replicate 100 250)

readings = U.map (gravity solution) locations


-- next lets specify the genotype
instance Genome Double where
  fitness v
      | r < 0 = 0
      | otherwise = 1000 / (1 + 1e10*sse)
    where
      r = v U.! 2
      x = v U.! 0
      z = v U.! 1
      ore = Ore x z r 2500
      field = U.map (gravity ore) locations
      error2 ((rx, ry), (fx, fy)) = (rx - fx)^2 + (ry - fy)^2
      sse = U.sum . U.map error2 $ U.zip readings field

ga = do
  n <- asks numberGens
  initPop <- randomPopulation
  finalPop <- foldl (>>=) (return initPop) (replicate n evolve)
  getBest finalPop

evolve pop = do
  fit <- calculateFitness pop
  off <- chooseParents fit pop >>= recombination >>= mutationReal
  elitism fit pop off
 
-- set the GA parameters
conf = GAConfig 100 10000 3 0.1 0.3

main = withSystemRandom (runReaderT ga . conf) >>= print
