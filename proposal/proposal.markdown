# Project Goal

This project aims to investigate evolutionary algorithms (EAs) such as
genetic algorithms (GAs) in relation to the production of geological
models. The main focus is on EAs, their implementation in the context
of geomodelling and how effective they are at producing solutions. The
goal is not to introduce a complete system for producing industrial
quality geological models from real data (many such systems already
exist) but rather to investigate the applicability of EAs in this
area.


# Background

## Geological Modelling

Mapping geological features under the earths surface is an important
task not only for academic reasons but also for commercial and
economical reasons. For example resivoir engineers in the mining, oil
and gas industries use geological models as a staring point to create
simulations that guide the development of the resivoir and the
eventual exctraction of material. Methods for producing high quality
geological models are therefore sought after and actively researched.

The geological models can be constructed from various geophysical sources
such as gravitational and magnetic field measurements, Downhole drill
samples and seismic serveys [@Dmod]. Often the field measurements are
collected using airbourne serveying techniques. However constructing
acurate 3D models from magnetic and gravitational data alone is not
straightforward and the solutions are not even be unique. Nettleton
stated in 1942 (as quoted in [@Dmod]).

> Unless certain controls other than the gravity and magnetic data are
> available the inherent ambiguities of the physically possible
> distributions of material which can produce the observed effects make
> accurate calculations meaningless even though the geophysical data may
> be of any desired precision.

Other information is required to eliminate unviable solutions.
The ability to store a collection of possible solutions is a fundamental
concept of EAs.

## Evolutionary Algorithms

Simply put Evolutionary Algorithms are based off the ideas of natural
evolution established by Darwin. The principle is described in [@ec]
as a stochastic trial-and-error style problem solving process. This does
not mean that the method isn't powerful because one only has to look
at the wide range of species that inhabit our world, each with traits
designed and honed to survive in a range of harsh environments. The
following points describe the process that govern all EAs.

1. An initial random population is created
2. The fitness of each individual is evaluated against the enviroment
3. A subset of the population is chosen to become parents 
4. Through various operations (such as recombination and mutation) new
   individuals are created
5. The new population is then tested against the enviroment. The
   fitness of each individual is evaluated
6. A group of surivors is chosen to become the next generation
7. Steps 3. through 6. are repeated until there is an acceptable
   solution

The process of selecting both parents and survivors according to how
well they handle the enviroment is the driving force behind EAs. This
process constantly promotes better solutions until an optimal or
acceptable solution is found. However this alone is not enough. Some
source of variation is required in order to generate new individuals.
This is handled through the so called genetic operators such as
mutation and recombination. This source of genetic variation not only
allows for better individuals not seen in the initial populatioin but
it also gives the algorithm the ability to search the entire solution
space and avoid the problem of getting stuck in local mixima.

# Conclusion

Conclude

\pagebreak <!--this puts refs on seperate page-->

# References
<!-- pandoc handles the refs -->

