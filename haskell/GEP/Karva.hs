{-# LANGUAGE OverloadedStrings #-}

module GEP.Karva
( executeChromosome
, splitChromosome
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString)

import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U

import qualified Data.Map as M 
import Data.Map ( Map )
import Data.Tree

import Data.Char ( digitToInt )

import Control.Monad.State
import Control.Monad.Reader

import Control.Applicative ( (<$>) )

import GEP.Types
import GEP.Config
import GEP.Monads

import System.Log.Logger 

-- in GEP-RNC each gene represents a valid program. often these
-- programs evaluate to floating point numbers used, for example as
-- parameters of a model to be optimized. The fitness also relies on
-- the programs output. 
--
-- Now i had a lot of trouble trying to find a solution that parses
-- the gene bytestream to a tree structure and then evaluates the tree
-- using a recursive definition. My first attempt was to use
-- parsec/attoparsec but thanks to the people at stackoverflow.com who
-- set me in ther right direction. See here
-- http://stackoverflow.com/questions/11869206/parsing-karva-notation-in-haskell
--
-- The heart of the problem is tree unfolding BREADTH-FIRST!!!
-- thankfully the Data.Tree exports a function for doing this.  we
-- will handle the parsing with attoparsec. for now we will hardcode
-- the operators "+-*/", the function "?" and the Dc-alphabet as
-- "0123456789" into the parsing code. Note this is a hack just to get
-- something working and is by no means a good solution. 

-- this could be polymorphic to allow solutions that are not floats
data Instruction = UnaryOp (Double -> Double) |
                   BinaryOp (Double -> Double -> Double) |
                   Value Double


type Node = Instruction


instructs :: Map Char Instruction
instructs = M.fromList
  [('+', BinaryOp (+))
  ,('-', BinaryOp (-))
  ,('*', BinaryOp (*))
  ,('/', BinaryOp (/))
  ]


type PartialGene = (ByteString, ByteString, UVector Double)
  
randomOp :: State PartialGene Node 
randomOp = do
  (ss, rs, rncs) <- get
  let i = digitToInt $ BSC.head rs
  put (ss, BS.tail rs, rncs)
  return $ Value (rncs U.! i)

getOp :: Char -> State PartialGene Node
getOp '?' = randomOp
getOp c = return (instructs M.! c)


step :: Node -> State PartialGene (Node, [Node])
step node = do
    (ss, rs, rncs) <- get
    let (args, rest) = case node of
                        (BinaryOp _) -> BS.splitAt 2 ss
                        (UnaryOp _) -> BS.splitAt 1 ss
                        _ -> (BS.empty, ss)
    put (rest, rs, rncs)
    children <- mapM getOp (BSC.unpack args)
    return (node, children)

treeify :: State PartialGene (Tree Node)
treeify = do
  (ss, rs, rncs) <- get
  if BS.null ss
    then fail "empty list"
    else do
      put (BS.tail ss, rs, rncs)
      root <- getOp (BSC.head ss)
      unfoldTreeM_BF step root

splitGene :: Gene -> Stack PartialGene
splitGene (Gene bs rncs) = do
  hl <- asks headLength
  tl <- asks tailLength
  let (ss, rs) = BS.splitAt (hl + tl) bs
  return (ss, rs, rncs)



partByteString :: ByteString -> Int -> [ByteString]
partByteString bs l
  | l > BS.length bs = []
  | otherwise = let (s, rest) = BS.splitAt l bs 
                in s:partByteString rest l

partUVector :: UVector Double -> Int -> [UVector Double]
partUVector v l
  | l > U.length v = []
  | otherwise = let (s, rest) = U.splitAt l v
                in s:partUVector rest l

splitChromosome :: Chromosome -> Stack (Vector Gene)
splitChromosome (Chrome (Gene ss rncs)) = do
    gl <- asks geneLength
    rs <- asks nRandoms
    let vs = partUVector rncs rs
    let bs = partByteString ss gl
    return (B.fromList $ zipWith Gene bs vs)

evalTree :: Tree Node -> Double
evalTree (Node (Value v) _) = v
evalTree (Node (UnaryOp u) [t]) = u (evalTree t)
evalTree (Node (BinaryOp b) (lt:rt:[])) = b (evalTree lt) (evalTree rt)
evalTree _ = error "something went wrong"

executeGene :: Gene -> Stack Double
executeGene g = do
  pg <- splitGene g
  return (evalTree $ evalState treeify pg)

-- for optimization problems their is no need to link the genes with
-- operators. Each gene represents a parameter of the solution

executeChromosome :: Chromosome -> Stack Params
executeChromosome c = do
  genes <- splitChromosome c
  B.convert <$> B.mapM executeGene genes
 
main :: IO ()
main = do
  updateGlobalLogger "GEP.Karva" (setLevel NOTICE)
  noticeM "GEP.Karva" "[GEP.Karva] Done!"