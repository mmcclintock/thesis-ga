
import System.Environment ( getArgs, getProgName )
import System.IO
import System.Exit
import System.Console.GetOpt
import Text.Parsec
import Text.Parsec.String
import Numeric (readSigned, readFloat)
import Control.Applicative ( (<$), empty )
import Vectorial


-- to start with all data will be stored using haskell data structures at some
-- stage especially with numeric calculations better data structures maybe
-- needed (Data.Vector ..)


-- A position is a point in euclidean space. To start lets assume we are
-- dealing in 2D and for simplicity use cartesian cooridnates. The x
-- coordinate measures distance along the earth (assume flat locally) from
-- some base station/reference point and use the z coordinate to measure
-- distance above/below the surface. So position is a 2D vector quantity
newtype Position = Pos { getPosComponents :: (X, Z) } deriving Show

instance Vectorial2D Position where
  toCartesian = getPosComponents
  fromCartesian = Pos


-- When the plane flys over it can measure a number of things other than its
-- position such as gravitational and magnetic fields. Whether the
-- instrumentation reads field strength (scalar field) or the complete field
-- (vector field) is unknown. For simplicity again I'll asume the 2d case and
-- that the plane reads two field components in x and z.
newtype Field = Field { getFieldComponents :: (X, Z) } deriving Show


instance Vectorial2D Field where
  toCartesian = getFieldComponents
  fromCartesian = Field

-- Flyover data is vital to the algorithm. Basically it is used to calculate
-- how good a possible solution is. In other words it's needed to evaluate the
-- fitness function. Esentially it is list of (position, field) readings.
newtype Flyover = Flyover { getList :: [(Position, Field)] } deriving Show

sampleFlyover :: Flyover
sampleFlyover = Flyover [ (Pos (0.0, 100.0), Field (6.0, -6.0))
                        , (Pos (10.0, 100.0), Field (0.0, -10.0))
                        ]


-- for the sake of simplicity lets assume that the ore deposit is spherical
-- with a given radius and homogenous with a given density. The gravitational 
-- field can be replaced by that of a point mass.
data Deposit = Spherical { position :: Position
                         , radius :: Double
                         , density :: Double
                         } deriving Show

volume :: Deposit -> Double
volume = (4/3*pi*) . (^2) . radius

mass :: Deposit -> Double
mass d = (density d) * (volume d)

-- gravitational field of a deposit at any position outside the deposit
grav :: Deposit -> Position -> Field
grav dep p = fromCartesian . toCartesian $ k <*> d
  where
    bigG = 6.673e-11
    m = mass dep
    d = (position dep) <-> p
    k = bigG * m / (mag d) ^ 3

-- the fitness function compares the fields at each flyover reading and
-- accumulates the error
fitness :: Deposit -> Flyover -> Double
fitness d (Flyover rs) = foldl (+) 0 $ map (error) rs
  where error (p, f) = mag $ (grav d p) <-> f
    






















-- i will use parsec with strings to parse the data file. At some stage the
-- real data file may become large and this method may need updating.
parseFlyover :: FilePath -> IO Flyover
parseFlyover path = do
  res <- parseFromFile flyoverParser path
  case res of
    Left pe   -> hPutStrLn stderr ("hello") >> exitWith (ExitFailure 1) 
    Right fly -> return fly

flyoverParser = do
  rs <- endBy parseReading (char '\n')
  return (Flyover rs)

parseReading = do
  vs <- sepBy parseFloat (char ',')
  case vs of
    (x:z:fx:fz:[]) -> return (Pos (x,z), Field (fx, fz))
    otherwise      -> unexpected "4 fields only"

parseFloat = do
  s <- getInput
  case readSigned readFloat s of
    [(n,s')]  -> n <$ setInput s'
    _         -> empty


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


-- config is essentially a collection of all parameters and input data
data Config = Config { flyover :: IO Flyover
                     }

defaultConfig :: Config
defaultConfig = Config { flyover = return sampleFlyover }

-- options
options :: [OptDescr (Config -> IO Config)]
options = [ Option "i" ["input"] 
              (ReqArg (\arg con -> return con { flyover = parseFlyover arg }) "FILE")
              "Input Flyover File"
          , Option "h" ["help"]
              (NoArg (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo (intro prg) options) 
                exitWith ExitSuccess))
              "Show Help"
          ]
        where intro prg = "\n" ++ prg ++ " is a program that uses evolutionary algorithms to attempt to model ore deposits from flyover data of gravitational/magnetic fields.\n"

                    
main = do
  args <- getArgs

  let (actions, nonOptions, errors) = getOpt Permute options args

  config <- foldl (>>=) (return defaultConfig) actions
  fly <- flyover config

  putStrLn $ show fly
