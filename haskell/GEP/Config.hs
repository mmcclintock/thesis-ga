{-# LANGUAGE OverloadedStrings #-}

module GEP.Config
( Config (..)
, tailLength
, dcLength
, geneLength
, nRandoms
) where

import qualified Data.Map as M 
import Control.Applicative ( (<$>) )
import Test.QuickCheck

import GEP.Types

data Config = Config { name               :: String
                     , noChromosomes      :: Int
                     , genesPerChromosome :: Int
                     , headLength         :: Int
                     , dcAlphabet         :: DCMap
                     , rangeRNC           :: Range
                     , operators          :: OpMap
                     , terminals          :: TermMap
                     , mutationRate       :: Double
                     } deriving Show

instance Arbitrary Config where
  arbitrary = do
      nc <- choose (1,100)
      gpc <- choose (1,10)
      hl <- choose (1,50) 
      dc <- arbitrary
      ts <- insert dc <$> arbitrary
      rr <- arbitrary
      mr <- choose (0.0,1.0)
      return $ Config "Test" nc gpc hl dc rr sampleOps ts mr
    where
      insert dc tmap
        | M.null . unDCMap $ dc = tmap
        | otherwise = TermMap . M.insert '?' RNC . unTermMap $ tmap
 

sampleOps :: OpMap
sampleOps = M.fromList
  [('+', BinaryOp (+))
  ,('-', BinaryOp (-))
  ,('*', BinaryOp (*))
  ,('/', BinaryOp (/))
  ]

maxArity :: OpMap -> Int
maxArity = maximum . map arity . M.elems
  where 
    arity (UnaryOp _) = 1
    arity (BinaryOp _) = 2

tailLength :: Config  -> Int
tailLength c = headLength c * (maxArity (operators c) - 1) + 1

dcLength :: Config -> Int
dcLength = tailLength

geneLength :: Config -> Int
geneLength c = headLength c + 2 * tailLength c

nRandoms :: Config -> Int
nRandoms = M.size . unDCMap . dcAlphabet

prop_rnc c = nodc == noRNC
  where
    nodc = M.null . unDCMap . dcAlphabet $ c
    noRNC = not . M.member '?' . unTermMap . terminals $ c

main :: IO ()
main = do
  putStrLn "[GEP.Config] Running Tests ..."
  quickCheck prop_rnc
