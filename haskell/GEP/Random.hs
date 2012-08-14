module GEP.Random
( randomSymbol
, randomGene
, randomChromosome
, randomPop
) where

import GEP.Types
import GEP.Config
import GEP.Monads

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Random

import Control.Applicative ( (<$>) )

import Data.Monoid

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U

import Test.QuickCheck

randomSymbol :: MonadRandom m => Alphabet -> m Char
randomSymbol s = do
  i <- getRandomR (0, BS.length s - 1)
  return (BSC.index s i)

randomGene :: Stack Gene
randomGene = do
  -- head domain
  hl <- asks headLength
  ha <- asks headAlphabet
  h <- BSC.pack <$> replicateM hl (randomSymbol ha)

  -- tail domain
  tl <- asks tailLength
  ta <- asks tailAlphabet
  t <- BSC.pack <$> replicateM tl (randomSymbol ta)

  -- Dc domain
  dcl <- asks dcLength
  dca <- asks dcAlphabet
  dc <- BSC.pack <$> replicateM dcl (randomSymbol dca)

  -- RNC's
  nr <- asks nRandoms
  rr <- asks rangeRNC
  rncs <- U.fromList <$> replicateM nr (getRandomR rr)

  return $ Gene (BS.concat [h, t, dc]) rncs

randomChromosome :: Stack Chromosome
randomChromosome = do
  ng <- asks numberGenes
  gs <- replicateM ng randomGene
  return $ Chrome $ mconcat gs

-- generate a random population
randomPop :: Stack ()
randomPop = do
  n <- asks popSize
  pop <- B.fromList <$> replicateM n randomChromosome
  put pop

main :: IO ()
main = putStr "[GEP.Random] Done!"
