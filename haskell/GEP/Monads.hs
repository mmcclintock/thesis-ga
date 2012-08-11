module GEP.Monads
( Stack
) where


import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Random

import GEP.Types
import GEP.Config

import System.Log.Logger

{-type Ran = Rand StdGen-}
type RanT = RandT StdGen
{-type CRead = Reader Config-}
type CReadT = ReaderT Config
type Population = Vector Chromosome
{-type Pop = State Population-}
type PopT = StateT Population
type Stack = PopT (CReadT (RanT IO))

main :: IO ()
main = do
  updateGlobalLogger "GEP.Monads" (setLevel NOTICE)
  noticeM "GEP.Monads" "[GEP.Monads] Done!"
