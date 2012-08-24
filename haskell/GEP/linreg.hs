{-# LANGUAGE OverloadedStrings #-}

import GEP.Types
import GEP.Config

import qualified Data.Vector.Unboxed as U


xvals :: UVector Double
xvals = U.fromList [1.0, 2.0, 3.0]

yvals :: UVector Double
yvals = U.fromList [1.0, 2.0, 3.0]
-- the answer is obviously y = x (m = 1.0, c = 0.0)

-- need two genes
lineConf :: Config
lineConf = Config 
  { headLength  = 10
  , operators   = "+-"
  , terminals   = "?"
  , dcAlphabet  = "01234"
  , rangeRNC    = (-10.0, 10.0)
  , numberGenes = 2
  , fitnessFunc = fit
  , popSize     = 10
  , mutationRate = 0.4
  }

fit :: Params -> Fitness
fit ps = 1000.0 / (1 + mse)
  where 
    m = U.unsafeHead ps
    c = U.unsafeIndex ps 1
    ys = U.map (\x -> m*x + c) xvals
    mse = U.sum . U.map (^(2 :: Int)) $ U.zipWith (-) ys yvals

-- GEP is now set and ready to do linear regression
-- Not Quite !!

main :: IO ()
main = putStr "linreg Done!"
