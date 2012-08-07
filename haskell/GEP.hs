{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString as BS
import Data.Word ( Word8 )
import Data.Char (intToDigit)

import qualified Data.Vector.Unboxed as V
import qualified Data.Map as M

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Char8 ( Parser, Result )

import Control.Monad
import Control.Monad.Random
import Control.Applicative ( pure, (<$>) )
import Data.Monoid


-- This module implements the GEP-RNC algorithm as detailed in the
-- book by Cândida Ferreira, Gene Expression Programming.

-- In GEP-RNC There are 3 domains per gene: the head, the tail and the
-- Dc domain. 

-- The head domain represents the expression tree so it can contain
-- operator symbols (+-*/) as well as terminal symbols which may
-- represent variables, numerical constants or random number
-- constants. The length (h) of the head section (number of symbols)
-- is an independant parameter for the GEP algorithms.

-- The tail domain can only contain terminal characters. Its purpose
-- is to ensure every operator in the head domain is fully applied.
-- This requirement sets its length (t) as a function of the head
-- length.

-- The Dc domain is designed for handling the random number constants.
-- This section is seperate from the head/tail so is free to have its
-- own alphabet of any symbols. The symbols form the keys that map to
-- a set of randomly generated numbers (RNCs). The symbols are common
-- accross genes but the RNCs are not. That is each gene carries its
-- own set of RNCs. The number (n) of Dc-specific symbols is another
-- independant parameter for the GEP-RNC algorithm.

-- The random terminal '?' has a special meaning in the head domain.
-- Every '?' is replaced sequentially by an RNC starting with the RNC
-- identified by the first Dc-symbol and continuing down the Dc domain
-- until there are no more '?' characters in the head domain. This
-- means the Dc domain must have the same length (t) as the tail
-- domain for the same reason.

-- Each Gene is represented by (h+2t) symbols along with n RNCs.
-- In haskell lets use strict bytestrings for the symbols and unboxed
-- vectors for the RNCs

type RNCs = V.Vector Double
data Gene = Gene BSC.ByteString RNCs deriving Show

-- Multigenic systems are useful for optimization problems where each
-- gene represents a parameter of the problem. The collection of all
-- genes that represent a solution form a chromosome. For example if
-- the problem has two parameters than two genes per chromosome are
-- required. 

-- In terms of a haskell representation chromosomes can be
-- represented using the same gene-like structure the only difference
-- is that for multigenic systems the bytestring and vector will be
-- larger. Because the length of each gene is known the bytesting and
-- vector can be split into respective genes when needed. Indeed when
-- only one gene is present the terms gene and chromosome are
-- interchangeable. The idea of chromosome as a large gene formed by
-- joining smaller genes points to a monoid.

type Chromosome = Gene

instance Monoid Gene where
  mempty = Gene BSC.empty V.empty
  mappend (Gene b1 v1) (Gene b2 v2) = 
    Gene (BS.append b1 b2) (v1 V.++ v2)

-- While efficient to represent chromosomes by two single structures
-- (a bytestring and a vector) especially for operations like
-- recombinations, many operations require knowledge of the individual
-- genes and their domains. Also in order to construct the genotype
-- (expression tree) knowledge of the symbols and what they represent
-- is required. However all this information is decided ahead of time
-- before the algorithm starts so it can be stored along with all the
-- other algorithm parameters. 











-- information to interpret gene/chromosome
data Genome = Genome { maxArity :: Int
                     , headSize :: Int
                     , terminalSet :: BSC.ByteString
                     , functionSet :: BSC.ByteString
                     , numberRNC :: Int
                     , rangeRNC :: (Double, Double)
                     , numberGenes :: Int
                     } deriving Show

tailSize :: Genome -> Int
tailSize g = (headSize g) * ((maxArity g) - 1)  + 1

geneSize :: Genome -> Int
geneSize g = (headSize g) + (tailSize g)

symbolSet :: Genome -> SymbolSet
symbolSet g = BS.append (terminalSet g) (functionSet g)


-- example genome decleration
ge = Genome 2 15 "?" "+-*/" 5 (-100.0,100.0) 2

randomSymbol :: SymbolSet -> Rand StdGen Word8
randomSymbol s = do
  i <- getRandomR (0, (BS.length s) - 1)
  return (BSU.unsafeIndex s i)

randomInteger :: (Int, Int) -> Rand StdGen Char
randomInteger r = do
  i <- getRandomR r
  return (intToDigit i)

-- generate random gene from genome
randomGene :: Genome -> Rand StdGen Gene
randomGene g = do
    head <- replicateM hl (randomSymbol ss)
    tail <- replicateM tl (randomSymbol ts)
    rts <- replicateM nr (randomInteger (0, nr))
    rncs <- replicateM nr (getRandomR rr)
    let bs = BS.concat [(BS.pack head), (BS.pack tail), (BSC.pack rts)]
    let rnv = V.fromList rncs
    return $ Gene bs rnv
  where 
    hl = headSize g
    tl = tailSize g
    ss = symbolSet g
    ts = terminalSet g
    nr = numberRNC g
    rr = rangeRNC g

-- generate random chromosome from genome
randomChromosome :: Genome -> Rand StdGen Chromosome
randomChromosome g = do
    gs <- replicateM (numberGenes g) (randomGene g)
    return $ mconcat gs


termChar = A.char '?'
opChar = A.satisfy (A.inClass "+-*/")


buildList :: Parser [[Char]]
buildList = init >>= build
  where
    init = (\x -> [[x]]) <$> A.anyChar


build :: [[Char]] -> Parser [[Char]]
build xs = if n > 0 
           then ((:xs) <$> (A.count n A.anyChar)) >>= build
           else pure xs
  where 
    value '+' = 2
    value '-' = 2
    value '*' = 2
    value '/' = 2
    value '?' = 0
    n = sum $ map value (head xs)


unsafeGetDone :: Result [[Char]] -> [[Char]]
unsafeGetDone (A.Done _ r) = reverse r


main = do
  putStrLn "Done!"
