{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import BasicPrelude
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
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

solution :: Ore
solution = Ore 500 (-1000) 250 2500

locations :: Vector (X, Z)
locations = V.zip (V.enumFromN 0 100) (V.replicate 100 250)

readings :: Vector (X, Z)
readings = V.map (gravity solution) locations


-- next lets specify the genotype

newtype Genotype = Genotype { unGenotype :: UVector Double } deriving Show

instance GAGenotype Genotype Double where

  toVec = unGenotype
  fromVec = Genotype

  fitness g
      | r < 0 = 0
      | otherwise = 1000 / (1 + 5e11*sse)
    where
      v = toVec g
      r = v U.! 2
      x = v U.! 0
      z = v U.! 1
      ore = Ore x z r 2500
      field = V.map (gravity ore) locations
      error2 ((rx, ry), (fx, fy)) = (rx - fx)^2 + (ry - fy)^2
      sse = V.sum . V.map error2 $ V.zip readings field

-- set the GA parameters
conf :: GAConfig
conf = GAConfig { popSize = 120
                , numberGens = 3000
                , vecSize = 3
                , mutationRate = 0.2
                , recombinationRate = 0.3
                }


ga :: GAStack (Genotype, Fitness)
ga = do
  n <- asks numberGens
  initPop <- randomPopulation
  finalPop <- foldl (>>=) (return initPop) (replicate n evolve)
  getBest finalPop

evolve :: Vector Genotype -> GAStack (Vector Genotype)
evolve pop = do
  fit <- calculateFitness pop
  off <- chooseParents fit pop >>= recombination >>= mutationReal
  elitism fit pop off
 
main :: IO ()
main = do
  putStrLn "Starting GA!"

  gen <- newStdGen
  putStrLn $ "Generated random seed: " ++ show gen

  print (runGAStack ga conf gen)


















