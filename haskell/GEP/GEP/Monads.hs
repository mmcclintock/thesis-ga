module GEP.Monads
( Stack
, get
, put
, asks
, getRandom
, getRandomR
, runStack
, Ran
) where


import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Random

import GEP.Types
import GEP.Config

type Ran = Rand StdGen
type CReadT = ReaderT Config
type Population = Vector Chromosome
type PopT = StateT Population
type Stack = PopT (CReadT Ran)

runStack :: Stack a -> Population -> Config -> StdGen -> a
runStack s p c = evalRand (runReaderT (evalStateT s p) c)

main :: IO ()
main = putStrLn "[GEP.Monads] Done!"
