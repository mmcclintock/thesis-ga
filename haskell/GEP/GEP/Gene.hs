{-# LANGUAGE FlexibleInstances #-}

module GEP.Gene
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U

import Test.QuickCheck
import Test.QuickCheck.Gen


randomSymbol :: MonadRandom m => String -> m Char
randomSymbol s = do
  i <- getRandomR (0, length s - 1)
  return $ s !! i

randomGene :: Stack Gene
randomGene = do
  -- head domain
  hl <- asks headLength
  ha <- asks headChars
  h <- BSC.pack <$> replicateM hl (randomSymbol ha)

  -- tail domain
  tl <- asks tailLength
  ta <- asks tailChars
  t <- BSC.pack <$> replicateM tl (randomSymbol ta)

  -- Dc domain
  dcl <- asks dcLength
  dca <- asks dcChars
  dc <- if null dca
    then return BS.empty
    else BSC.pack <$> replicateM dcl (randomSymbol dca)

  -- RNC's
  nr <- asks nRandoms
  rr <- asks rangeRNC
  rncs <- U.fromList <$> replicateM nr (getRandomR . unRange $ rr)

  return $ Gene h t dc rncs

randomChromosome :: Stack Chromosome
randomChromosome = do
  ng <- asks genesPerChromosome
  gs <- replicateM ng randomGene
  return $ B.fromList gs

-- generate a random population
randomPop :: Stack ()
randomPop = do
  n <- asks noChromosomes
  pop <- B.fromList <$> replicateM n randomChromosome
  put pop


newtype ConfGenes = CGs (Config, [Gene]) deriving Show


ranToGen :: Ran a -> Gen a
ranToGen rm = MkGen (\g _ -> evalRand rm g)

instance Arbitrary ConfGenes where 
  arbitrary = do
      conf <- arbitrary
      let geneRan = runReaderT (evalStateT randomGene B.empty) conf
      genes <- (vectorOf 3) $ ranToGen geneRan 
      return $ CGs (conf, genes)



prop_lengths :: ConfGenes -> Bool
prop_lengths (CGs (c, genes)) = all (cls ==) (map ls genes)
  where 
    bsl = BS.length
    ls (Gene h t dc rncs) = (bsl h, bsl t, bsl dc, U.length rncs)
    cls = (headLength c, tailLength c, dcLength c, rncLength c)

   
main :: IO ()
main = do
  putStrLn "[GEP.Random] Running Tests!"
  quickCheck prop_lengths


