{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U

import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Random
import Control.Monad.State
import Control.Applicative ( (<$>) )

import System.Log.Logger

import GEP.Types
import GEP.Config
import GEP.Karva
import GEP.Monads
import GEP.Random

-- calculate fitness of every individual 
popFitness :: Stack (Vector Fitness)
popFitness = do
  pop <- get
  c <- asks fitnessFunc
  params <- B.mapM executeChromosome pop
  return (B.map c params)

-- selection is a critical part of GEP and together with replication
-- sets the stage for evolution. replication is trivial (especially in
-- haskell) and amounts to producing an exact copy of a chromosome.
-- selection is roulette based where each chromosome has a place on
-- the roulette who's size is proportional to its fitness. That way
-- individuals with greater fitness have better odds of being
-- replicated. The roulette wheel is spun N times to ensure the
-- population size remains constant.

roulette :: Vector Fitness -> Stack Chromosome
roulette fs = do
  pop <- get
  n <- getRandomR (0.0, B.sum fs)
  let acc (a, _) (f, c) = (a + f, c)
  let first = (0.0, B.unsafeHead pop)
  let csum = B.scanl acc first $ B.zip (B.convert fs) pop
  let rest = B.dropWhile (\(s, _) -> s <= n) csum
  return (snd . B.unsafeHead $ rest)

getBest :: Stack Chromosome
getBest = do
  f <- popFitness
  pop <- get
  let pf = B.zip pop f
  let best = B.maximumBy (\(_,f1) (_,f2) -> compare f1 f2) pf
  return (fst best)
  

selection :: Stack ()
selection = do
  f <- popFitness
  b <- getBest
  n <- asks popSize
  newpop <- B.replicateM n (roulette f) -- spin roulette n times
  put (B.cons b newpop) -- elitism

type Rate = Double

mutateSymbol :: Rate -> Alphabet -> Char -> Stack Char
mutateSymbol r a c = do
  n <- getRandom
  if n < r
    then randomSymbol a
    else return c

mutateGene :: Gene -> Stack Gene
mutateGene (Gene bs rncs) = do
  hs <- asks headLength
  tl <- asks tailLength
  mr <- asks mutationRate
  ha <- asks headAlphabet
  ta <- asks tailAlphabet
  da <- asks dcAlphabet
  let (h, rest) = BS.splitAt hs bs
  let (t, d) = BS.splitAt tl rest
  let mut = mutateSymbol mr
  nh <- mapM (mut ha) (BSC.unpack h)
  nt <- mapM (mut ta) (BSC.unpack t)
  nd <- mapM (mut da) (BSC.unpack d)
  return (Gene (BSC.pack $ concat [nh, nt, nd]) rncs)

mutateChromosome :: Chromosome -> Stack Chromosome
mutateChromosome c = do
  genes <- splitChromosome c
  mutants <- B.mapM mutateGene genes
  return (Chrome . mconcat $ B.toList mutants)

mutation :: Stack ()
mutation = do
  pop <- get
  newpop <- B.mapM mutateChromosome pop
  put newpop

evolve :: Stack ()
evolve = do
  selection -- decide which chromosomes make it
  mutation  -- mutation can be done better

gep :: Int -> Stack (Vector Double)
gep n = do
  randomPop
  replicateM_ n evolve
  popFitness

main :: IO ()
main = do
  updateGlobalLogger "GEP" (setLevel NOTICE)
  noticeM "GEP" "[GEP] Done!"
