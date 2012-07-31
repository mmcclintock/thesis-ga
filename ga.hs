import System.Environment ( getArgs, getProgName )
import System.IO
import System.Exit
import System.Console.GetOpt

-- type synonim for 2d quantities with components in x,z directions
type XZPair = (Double, Double)


-- A position is defined by three components in 3d eucledian space. for
-- simplicity i'll start with 2 cartesian coordinates x and z.
newtype Pos = Pos { getPosComponents :: XZPair } deriving Show


-- When the plane flys over it can measure a number of things other than its
-- position such as gravatational and magnetic fields. Whether the
-- instrumentation reads field strength (scalar field) or the complete field
-- (vector field) is unknown. For simplicity again I'll asume the 2d case and
-- that the plane reads two field components in x and z.
newtype Field = Field { getFieldComponents :: XZPair } deriving Show


-- Flyover data is vital to the algorithm. Basically it is used to calculate
-- how good a possible solution is. In other words it's needed to evaluate the
-- fitness function. Esentially it is list of (position, field) readings.
newtype Flyover = Flyover { getList :: [(Pos, Field)] } deriving Show

sampleFlyover :: Flyover
sampleFlyover = Flyover [ (Pos (0.0, 100.0), Field (6.0, -6.0))
                        , (Pos (10.0, 100.0), Field (0.0, -10.0))
                        ]

parseFlyover :: FilePath -> IO Flyover
parseFlyover _ = return sampleFlyover


-- This program will except parameters such as an input data file on the 
-- command line. I'll process the options/arguments using GetOpt and the 
-- method discussed here:
-- http://www.haskell.org/haskellwiki/High-level_option_handling_with_GetOpt
--
-- to start lets say the flyover data is stored in an external text file
-- we can pass the filename in using the -i or --input option. The beauty of
-- the method described is that all the parsing/coversion of data is also part
-- of the option parsing. This means once the options have passed i'll have
-- the Flyover data ready to go.


data Config = Config { flyover :: IO Flyover
                     } 

defaultConfig :: Config
defaultConfig = Config { flyover = return sampleFlyover }

options :: [OptDescr (Config -> IO Config)]
options = [ Option "i" ["input"] 
              (ReqArg (\arg con -> return con { flyover = parseFlyover arg }) "FILE")
              "Input Flyover File"
          , Option "h" ["help"]
              (NoArg (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
              "Show Help"
          ]

                    
main = do
  args <- getArgs

  let (actions, nonOptions, errors) = getOpt Permute options args

  config <- foldl (>>=) (return defaultConfig) actions

  putStrLn "Okay"
