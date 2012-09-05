{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HsEA.GA
( Genome (..)
, GAConfig (..)
, randomPopulation
, calculateFitness
, chooseParents
, GAStack
, asks
, recombination
, mutationReal
, elitism
, getBest
, Fitness
, runReaderT
, withSystemRandom
) where 

import BasicPrelude

import System.Random.MWC
import Control.Monad.Reader
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

type Fitness = Double

class (Unbox e, Variate e) => Genome e where
  fitness :: UVector e -> Fitness
  
data GAConfig = GAConfig 
  { popSize :: Int
  , numberGens :: Int
  , vecSize :: Int
  , mutationRate :: Double
  , recombinationRate :: Double
  , randomGenerator :: GenIO
  }

type GAStack = ReaderT GAConfig IO

randomPopulation :: Genome e => GAStack (Vector (UVector e))
randomPopulation = do
  size <- asks vecSize
  gen <- asks randomGenerator
  ps <- asks popSize
  V.replicateM ps . U.replicateM size $ liftIO (uniform gen)

calculateFitness :: Genome e => Vector (UVector e) -> GAStack (UVector Fitness)
calculateFitness vg = return . U.convert . V.map fitness $ vg

spinRoulette :: Genome e => GenIO -> UVector Fitness -> Vector (UVector e) -> GAStack (UVector e)
spinRoulette gen fs vs = do
  let csum = U.scanl1' (+) fs
  r <- liftIO $ uniformR (0, U.last csum) gen
  let i = U.length $ U.takeWhile (< r) csum
  return $ V.unsafeIndex vs i

chooseParents :: Genome e => UVector Fitness -> Vector (UVector e) -> GAStack (Vector (UVector e))
chooseParents fit pop = do
  gen <- asks randomGenerator
  V.replicateM (V.length pop) (spinRoulette gen fit pop)

combine :: Genome e => Double -> GenIO -> Int -> (UVector e, UVector e) -> GAStack (UVector e, UVector e)
combine rr gen vs (p1, p2) = do
  r <- liftIO $ uniform gen
  if r < rr 
    then do
      cp <- liftIO $ uniformR (0, vs) gen
      let (h1, t1) = U.splitAt cp p1
      let (h2, t2) = U.splitAt cp p2
      return (h1 U.++ t2, h2 U.++ t1)
    else
      return (p1, p2)

recombination :: Genome e => Vector (UVector e) -> GAStack (Vector (UVector e))
recombination par = do
  rr <- asks recombinationRate
  gen <- asks randomGenerator
  vs <- asks vecSize

  let (mothers, fathers) = V.splitAt (V.length par `div` 2) par
  offspring <- V.mapM (combine rr gen vs) $ V.zip mothers fathers
  let (f, b) = V.unzip offspring
  if odd $ V.length par
    then return $ V.last par `V.cons` f V.++ b
    else return $ f V.++ b

mutateReal :: Int -> Double -> GenIO -> UVector Double -> GAStack (UVector Double)
mutateReal size mr gen vals = do
    bools <- U.replicateM size $ fmap (< mr) (liftIO (uniform gen))
    creep <- U.replicateM size $ liftIO $ uniform gen
    return . U.map mut $ U.zip3 bools creep vals
  where
    mut (True, creep, val) = val + creep
    mut (False, _   , val) = val

mutationReal :: Vector (UVector Double) -> GAStack (Vector (UVector Double))
mutationReal pop = do
  size <- asks vecSize
  gen <- asks randomGenerator
  mr <- asks mutationRate
  V.mapM (mutateReal size mr gen) pop

getBest :: Genome e => Vector (UVector e) -> GAStack (UVector e, Fitness)
getBest pop = return . V.maximumBy cmp $ V.zip pop fit
  where 
    fit = V.map fitness pop
    cmp (_,f1) (_,f2) = compare f1 f2

elitism :: Genome e => UVector Fitness -> Vector (UVector e) -> Vector (UVector e) -> GAStack (Vector (UVector e))
elitism fit pop off
  | (V.maximum . V.map fitness $ off) > bestParFit = return off
  | otherwise = return . V.cons bestPar . V.tail $ off
  where
    cmp (f1,_) (f2,_) = compare f1 f2
    (bestParFit, bestPar) = V.maximumBy cmp $ V.zip (V.convert fit) pop
