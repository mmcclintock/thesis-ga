{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString, pack)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B

import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Random
import Control.Monad.State
import Control.Applicative ( pure, (<$>) )

import Data.Word ( Word8 )
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Map ( Map )
import Data.Tree


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

type Vector = B.Vector
type UVector = U.Vector
type RNCs = UVector Double
type Symbols = ByteString
type Alphabet = Symbols

data Gene = Gene Symbols RNCs deriving Show

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

instance Monoid Gene where
  mempty = Gene BSC.empty U.empty
  mappend (Gene b1 v1) (Gene b2 v2) = 
    Gene (BS.append b1 b2) (v1 U.++ v2)

newtype Chromosome = Chrome { toGene :: Gene } deriving Show

-- While efficient to represent chromosomes by two single structures
-- (a bytestring and a vector) especially for operations like
-- recombinations, many operations require knowledge of the individual
-- genes and their domains. Also in order to construct the genotype
-- (expression tree) knowledge of the symbols and what they represent
-- is required. However all this information is decided ahead of time
-- before the algorithm starts so it can be stored along with all the
-- other algorithm parameters. 

-- because binary functions have different types to unary and ternary
-- operators its hard to define a mapping of operator symbol to
-- functions. For simplicity lets just support binary operators (+-*/)
-- this means maxArity = 2. 
--
data Config = Config { headLength  :: Int
                     , operators   :: Symbols
                     , terminals   :: Symbols
                     , dcAlphabet  :: Alphabet  -- sets n
                     , rangeRNC    :: (Double, Double)
                     , numberGenes :: Int
                     } deriving Show

tailLength c   = headLength c + 1
dcLength       = tailLength
geneLength c   = headLength c + 2 * tailLength c
nRandoms       = BS.length . dcAlphabet
headAlphabet c = BS.append (terminals c) (operators c)
tailAlphabet   = terminals


-- Obviously the GEP algorithm relies on the ability to produce random
-- numbers. To handle this in haskell we must pass around a random
-- generator but we can hide this in a state monad. 

type Ran = Rand StdGen

-- passing the config as the first parameter to every function is
-- annoying. This can be fixed by using the reader monad. The reader
-- monad is often used in combination with the Ran Monad and the
-- Pop monad so again we will define a transformer.

type CRead = Reader Config
type CReadT = ReaderT Config

-- to initialize the population the genotype needs to be filled with
-- random symbols. This random computation returns a random symbol
-- from a given alphabet,
randomSymbol :: Alphabet -> Ran Word8
randomSymbol s = do
  i <- getRandomR (0, BS.length s - 1)
  return (BSU.unsafeIndex s i)

-- to generate a random gene we need to use the config (handled by
-- Reader monad) and pass around the random generator (handled by
-- StdRand monad). We can use the Reader monad transformer to stack
-- the monads.

randomGene :: CReadT Ran Gene
randomGene = do
  -- head domain
  hl <- asks headLength
  ha <- asks headAlphabet
  h <- lift $ pack <$> replicateM hl (randomSymbol ha)

  -- tail domain
  tl <- asks tailLength
  ta <- asks tailAlphabet
  t <- lift $ pack <$> replicateM tl (randomSymbol ta)

  -- Dc domain
  dcl <- asks dcLength
  dca <- asks dcAlphabet
  dc <- lift $ pack <$> replicateM dcl (randomSymbol dca)

  -- RNC's
  nr <- asks nRandoms
  rr <- asks rangeRNC
  rncs <- lift $ U.fromList <$> replicateM nr (getRandomR rr)

  return $ Gene (BS.concat [h, t, dc]) rncs

-- generate random chromosome from config. Because genes are are
-- monoids to make a random chromosome we can just make multiple genes
-- and then concat them.
randomChromosome :: CReadT Ran Chromosome
randomChromosome = do
    ng <- asks numberGenes
    gs <- replicateM ng randomGene
    return $ Chrome $ mconcat gs



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

splitGene :: Gene -> CRead PartialGene
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

splitChromosome :: Chromosome -> CRead (Vector Gene)
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

executeGene :: Gene -> CRead Double
executeGene g = do
  pg <- splitGene g
  return (evalTree $ evalState treeify pg)

-- for optimization problems their is no need to link the genes with
-- operators. Each gene represents a parameter of the solution
type Params = UVector Double

executeChromosome :: Chromosome -> CRead Params
executeChromosome c = do
  genes <- splitChromosome c
  B.convert <$> B.mapM executeGene genes
  
-- the fitness of individuals drives evolution. for now lets assume
-- fitness is a number between 0 and 1000. 0 represents an unviable
-- solution while 1000 represents a perfect solution. The fitenss
-- function is obviously problem specific so we can't provide
-- definition here but we can define a class

type Fitness = Double

class GeneticModel a where

  -- construct a model
  mkModel :: Params -> a

  -- takes UVector of parameters and returns the fitneass
  fitness :: a -> Fitness

-- for testing, an example model is a straight line with 2 parameters,
-- the enviroment is data coordinates. we can provide a fitness
-- function related to the mean squared error and GEP-PO will perform
-- linear regression.

xvals = (U.fromList [1.0, 2.0, 3.0])
yvals = (U.fromList [1.0, 2.0, 3.0])
-- the answer is obviously y = x (m = 1.0, c = 0.0)

-- need two genes
lineConf :: Config
lineConf = Config 10 "+-*" "?" "01234" (-100.0,100.0) 2

newtype Line = Line { toDoubs :: (Double, Double) }

instance GeneticModel Line where

  mkModel ps = Line (m,c)
    where 
      m = U.unsafeHead ps
      c = U.unsafeIndex ps 1

  fitness (Line (m, c)) = 1000.0 / (1 + mse)
    where 
      ys = U.map (\x -> m*x + c) xvals
      mse = U.sum . U.map (^2) $ U.zipWith (-) ys yvals

-- GEP is now set and ready to do linear regression


-- so far we have dealt with: gene and chromosome representation,
-- configuration, creation random genes/chromosomes, gene/chromosome
-- execution and fitness calculation. What is left is the real nuts
-- and bolts of GEP the creation of new generations with selection and
-- modification operators. 
--
-- Throughout the GEP-Algorithm a record of the current population
-- must be kept at all times (we can used a boxed vector). Also the
-- next population is constructed from the last. This is an example of
-- a stateful computation where a state monad is useful. However we
-- will often want use the stateful population monad alongside other
-- monads like the Random monad. So we should also define a monad
-- transformer.

type Population = Vector Chromosome
type Pop = State Population
type PopT = StateT Population

-- selection is a critical part of GEP and together with replication
-- sets the stage for evolution. replication is trivial (especially in
-- haskell) and amounts to producing an exact copy of a chromosome.
-- selection is roulette based where each chromosome has a place on
-- the roulette who's size is proportional to its fitness. That way
-- individuals with greater fitness have better odds of being
-- replicated. The roulette wheel is spun N times to ensure the
-- population size remains constant.
 
{-popFitness :: Config -> Pop Fitness-}
{-popFitness c = do-}
  {-pop <- get-}
  {-params <- B.mapM executeChromosome pop-}
  {-models <- B.mapM mkModel params-}
  {-B.mapM fitness models-}


{-selection :: PopT Rand Population-}
{-selection = -}

-- fitness :: CReadT Pop (UVector Double)
-- evolve :: Population -> CReadT PopT Rand ()

main :: IO ()
main = putStrLn "Done!"
