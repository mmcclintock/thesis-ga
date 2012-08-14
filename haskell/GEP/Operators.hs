{-# LANGUAGE OverloadedStrings #-}

module GEP.Operators
( selection
) where

import qualified Data.Vector as B

import GEP.Types
import GEP.Monads
import GEP.Config
import GEP.Karva

-- In GEP there are a number of operators that act population wide to
-- introduce genetic variation with the goal of creating a better
-- population. These are
--
-- * selection (includes fitness calculation, reproduction, elitism)
-- * mutation
-- * inversion
-- * one point recombination
-- * two point recombination
-- * gene recombination

-- Selection is really what drives the algorithm to the best solution.
-- The idea is to reproduce the current generation randomly where an
-- individuals chance of reproduction is proportional to its fitness.
-- Unviable solutions have zero fitness therefore have no chance of
-- surviving to the next stage of the algorithm. After the new
-- population is created, a new concept, elitism is introduced where
-- the best individual from the last generation replaces the first
-- inidividual from the new population.

-- In terms of implementation the first step is to calculate the
-- fitness of each chromosome

fitness :: Stack (Vector Fitness)
fitness = do
  pop <- get
  ff <- asks fitnessFunc
  params <- B.mapM executeChromosome pop
  return (B.map ff params)

-- Next selection is implemented using the roulette wheel concept.
-- Create a roulette wheel where each individual has a segmented
-- proportional to their fitness.

type Wheel = (Double, Vector (Fitness, Chromosome))

roulette :: Vector Fitness -> Stack Wheel
roulette fs = do
  pop <- get
  let acc (a, _) (f, c) = (a + f, c)
  let first = (0.0, B.unsafeHead pop)
  let csum = B.scanl acc first $ B.zip (B.convert fs) pop
  return (B.sum fs, csum)

-- Spin the wheel to reproduce an individual

spin :: Wheel -> Stack Chromosome
spin (sum, csum) = do
  n <- getRandomR (0.0, sum)
  let rest = B.dropWhile (\(s, _) -> s <= n) csum
  return (snd . B.unsafeHead $ rest)

-- elitism requires us to be able to find the best individual

getBest :: Stack Chromosome
getBest = do
  fs <- fitness
  pop <- get
  let pf = B.zip pop fs
  let best = B.maximumBy (\(_,f1) (_,f2) -> compare f1 f2) pf
  return (fst best)

-- to perform selection spin the wheel n times. where n is the
-- population size. Then replace the first individual with the best
-- chromosome
 
selection :: Stack ()
selection = do
  wheel <- fitness >>= roulette  -- create the wheel
  b <- getBest
  n <- asks popSize
  newpop <- B.replicateM n (spin wheel) -- spin roulette n times
  let rem = B.unsafeTail newpop  -- discard the first individual
  put (B.cons b rem) -- put the best individual in its place



main :: IO ()
main = putStr "[GEP.Operators] Done!"

