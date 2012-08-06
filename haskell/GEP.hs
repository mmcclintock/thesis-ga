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


-- random number constants RNCs are supported by generating a number of random 


type Randoms = V.Vector Double


-- repr of gene
data Gene = Gene BSC.ByteString Randoms deriving Show
type Chromosome = Gene

type SymbolSet = BSC.ByteString





instance Monoid Gene where
  mempty = Gene BSC.empty V.empty
  mappend (Gene b1 v1) (Gene b2 v2) = Gene (BS.append b1 b2) (v1 V.++ v2)



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
