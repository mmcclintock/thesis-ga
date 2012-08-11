module GEP.Types
( UVector
, Vector
, Gene (..)
, Chromosome (..)
, Symbols
, Alphabet
, Fitness
, Params
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B

import Data.Monoid

import System.Log.Logger

type UVector = U.Vector
type Vector = B.Vector

type NCs = UVector Double
type Symbols = ByteString
type Params = UVector Double -- the parameters the genes encode
type Fitness = Double -- fitness is a number from 0 to 1000
type Alphabet = Symbols

data Gene = Gene Symbols NCs deriving Show

instance Monoid Gene where
  mempty = Gene BSC.empty U.empty
  mappend (Gene b1 v1) (Gene b2 v2) = 
    Gene (BS.append b1 b2) (v1 U.++ v2)

newtype Chromosome = Chrome { toGene :: Gene } deriving Show

main :: IO ()
main = do
  updateGlobalLogger "GEP.Types" (setLevel NOTICE)
  noticeM "GEP.Types" "[GEP.Types] Done!"
