{-# LANGUAGE OverloadedStrings #-}

module GEP.Config
( Config (..)
, tailLength
, dcLength
, geneLength
, nRandoms
, headAlphabet
, tailAlphabet
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString)

import GEP.Types

import System.Log.Logger

data Config = Config { headLength  :: Int
                     , operators   :: Symbols
                     , terminals   :: Symbols
                     , dcAlphabet  :: Alphabet  -- sets n
                     , rangeRNC    :: (Double, Double)
                     , numberGenes :: Int
                     , fitnessFunc :: Params -> Fitness
                     , popSize     :: Int
                     , mutationRate :: Double
                     }

-- FIX THE MAXARITY IN THIS EQUATION!!!!!
tailLength :: Config  -> Int
tailLength c = headLength c * (2 - 1) + 1

dcLength :: Config -> Int
dcLength = tailLength

geneLength :: Config -> Int
geneLength c = headLength c + 2 * tailLength c

nRandoms :: Config -> Int
nRandoms = BS.length . dcAlphabet

headAlphabet :: Config -> Alphabet
headAlphabet c = BS.append (terminals c) (operators c)

tailAlphabet :: Config -> Alphabet
tailAlphabet = terminals

main :: IO ()
main = putStr "[GEP.Config] Done!"






