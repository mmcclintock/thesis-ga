# haskell implementation

The program is run from the command line.

## Data File

input to the program is a collection of readings made by a plane as it surveys
an area. Each reading contains a position and a field reading. This data is
supplied in a csv file. When the program starts it will do the following to
obtain flyover data.

1. look for a file name given with the -i or --input options and attempt to
   parse it.
2. if 1. fails look in the current working directory for a file named
   default.csv and attempt to parse it.
3. otherwise generate sample flyover data.

## Configuration

The algorithm contains quite a few parameters that can be tuned for better
results.

1. The program defines sample configuration which will be used unless 2. or 3.
   overide them
2. The Program will look for a default.conf in the current working directory
   any parameters found will overide the sample configuration
3. Parameters given at the command line have highest priority and will overide
   values in 1. or 2.

parameters include initial population size and limits for deposit
size/position, a whole bunch of mutation/combination probabilities and
tolerance/number of evolutions.

## Output
TODO

## Comments

* parsing done with parsec.
* where possible handle randomness with state monad
* evolution is a stateful computation
* evolution should be handled by selecting the best individuals then using a
  probability tree to decide mutations/combinations
* traversal of the decision tree should use the random generator and hence is
  stateful
* once traversal ends the result should be a stateful computation that
  consumes the population but returns the mutations/creations/survivors.
* With this in mind each stage of evolution involves consuming the old
  population and creating a new one in the process.

