{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module HsEA.GA
( GAGenotype (..)
, GAConfig (..)
, randomIndividual
, randomPopulation
, calculateFitness
, chooseParents
, GAStack (..)
, runGAStack
, newStdGen
, asks
, recombination
, mutationReal
, elitism
, getBest
, Fitness
) where 

import BasicPrelude

import Control.Monad.Random
import Control.Monad.Reader
-- import Control.Monad.State
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

type Fitness = Double

class (Unbox e, Random e) => GAGenotype g e | g -> e where
  toVec :: g -> UVector e
  fromVec :: UVector e -> g
  fitness :: g -> Fitness
  
data GAConfig = GAConfig 
  { popSize :: Int
  , numberGens :: Int
  , vecSize :: Int
  , mutationRate :: Double
  , recombinationRate :: Double
  } deriving Show

newtype GAStack a = GAStack { 
  runStack :: ReaderT GAConfig (Rand StdGen) a
  } deriving (Functor, Monad, MonadReader GAConfig, MonadRandom)

runGAStack :: GAStack a -> GAConfig -> StdGen -> a
runGAStack stack conf = evalRand (runReaderT (runStack stack) conf)

randomIndividual :: GAGenotype g e => GAStack g
randomIndividual = do
  vs <- asks vecSize
  GAStack $ fromVec . U.fromList . take vs <$> getRandoms

randomPopulation :: GAGenotype g e => GAStack (Vector g)
randomPopulation = do
  ps <- asks popSize
  V.replicateM ps randomIndividual


calculateFitness :: GAGenotype g e => Vector g -> GAStack (Vector Fitness)
calculateFitness vg = return (V.map fitness vg)

spinRoulette :: GAGenotype g e => Vector (Fitness, g) -> GAStack g
spinRoulette fv = do
    let cumsum = V.scanl1' (\(acc, _) (f, g) -> (acc+f, g)) fv
    r <- getRandomR (0, fst . V.last $ cumsum)
    let leftovers = V.dropWhile (\(acc, _) -> acc < r) cumsum
    return (snd . V.head $ leftovers)

chooseParents :: GAGenotype g e => Vector Fitness -> Vector g -> GAStack (Vector g)
chooseParents fit pop = V.replicateM (V.length pop) (spinRoulette $ V.zip fit pop)

combine :: GAGenotype g e => (g, g) -> GAStack (g, g)
combine (p1, p2) = do
  rr <- asks recombinationRate
  r <- getRandom
  if r < rr 
    then do
      vs <- asks vecSize
      cp <- getRandomR (0, vs)
      let (h1, t1) = U.splitAt cp $ toVec p1
      let (h2, t2) = U.splitAt cp $ toVec p2
      return (fromVec $ h1 U.++ t2, fromVec $ h2 U.++ t1)
    else
      return (p1, p2)

recombination :: GAGenotype g e => Vector g -> GAStack (Vector g)
recombination par = do
  let (mothers, fathers) = V.splitAt (V.length par `div` 2) par
  offspring <- V.mapM combine $ V.zip mothers fathers
  let (f, b) = V.unzip offspring
  if odd $ V.length par
    then return $ V.last par `V.cons` f V.++ b
    else return $ f V.++ b

mutateReal' :: Double -> GAStack Double
mutateReal' n = do
  mr <- asks mutationRate
  r <- getRandom
  if r < mr
    then do
      cm <- getRandomR (-10.0, 10.0)
      return (n + cm)
    else return n

mutateReal :: (GAGenotype g Double) => g -> GAStack g
mutateReal g = fromVec <$> (U.mapM mutateReal' . toVec $ g)

mutationReal :: GAGenotype g Double => Vector g -> GAStack (Vector g)
mutationReal = V.mapM mutateReal

getBest :: GAGenotype g e => Vector g -> GAStack (g, Fitness)
getBest pop = return . V.maximumBy cmp $ V.zip pop fit
  where 
    fit = V.map fitness pop
    cmp (_,f1) (_,f2) = compare f1 f2

elitism :: GAGenotype g e => Vector Fitness -> Vector g -> Vector g -> GAStack (Vector g)
elitism fit pop off
  | (V.maximum . V.map fitness $ off) > bestParFit = return off
  | otherwise = return . V.cons bestPar . V.tail $ off
  where
    cmp (f1,_) (f2,_) = compare f1 f2
    (bestParFit, bestPar) = V.maximumBy cmp $ V.zip fit pop
 
main :: IO ()
main = putStrLn "Done!"
