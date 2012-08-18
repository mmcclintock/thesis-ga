module GEP.Types
( UVector
, Vector
, Gene (..)
, Chromosome
, Alphabet
, Fitness
, Params
, Operator (..)
, OpMap
, Terminal (..)
, TermMap (..)
, DCMap (..)
, Range (..)
) where

import Control.Applicative ( (<$>) )
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import qualified Data.Map as M
import Test.QuickCheck

-- Convenience synonyms
type UVector = U.Vector
type Vector = B.Vector


type Alphabet = BSC.ByteString
type NCs = UVector Double

type Head = Alphabet
type Tail = Alphabet
type DC   = Alphabet
type RNCs = NCs

data Gene = Gene Head Tail DC RNCs deriving Show

type Chromosome = Vector Gene

type Params = UVector Double -- the parameters the genes encode
type Fitness = Double -- fitness is a number from 0 to 1000

-- could be polymorphic
data Operator = UnaryOp (Double -> Double) |
                BinaryOp (Double -> Double -> Double)
                
instance Show Operator where
  show (UnaryOp _) = "(Double -> Double)"
  show (BinaryOp _) = "(Double -> Double -> Double)"

type OpMap = M.Map Char Operator

data Terminal = Value Double | RNC deriving Show

newtype TermMap = TermMap { unTermMap :: M.Map Char Terminal} deriving Show

newtype DCMap = DCMap { unDCMap :: M.Map Char Int} deriving Show

newtype Range = Range { unPair :: (Double, Double) } deriving Show

instance Arbitrary Range where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ if a <= b then Range (a,b) else Range (b,a)

instance Arbitrary Terminal where
  arbitrary = Value <$> arbitrary
  
instance Arbitrary DCMap where
  arbitrary = do
    a <- listOf $ elements symbols
    return (DCMap . M.fromList $ zip a [0..])

symbols :: String
symbols = ['a'..'z'] ++ ['A'..'Z'] ++ "1234567890"

instance Arbitrary TermMap where
  arbitrary = do
    a <- listOf1 . elements $ symbols 
    b <- vectorOf (length a) arbitrary
    return (TermMap . M.fromList $ zip a b)

main :: IO ()
main = putStr "[GEP.Types] Done!"
