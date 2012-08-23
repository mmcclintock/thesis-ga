# Topic Definition

## Project Goal

This project aims to investigate evolutionary algorithms (EAs) in
relation to the production of geological models. The main focus is on
EAs, their implementation in the context of geological modelling and
how effective they are at producing solutions. The goal is not to
introduce a complete system for producing industrial quality
geological models from real data (many such systems already exist) but
rather to implement a system that allows the study of the
applicability of EAs in this area. 

## Project Outline

The first objective of the  project is to solve the following problem
using computational methods.

> Consider an ore deposit which is modelled by an ellipsiod with a
> uniform density greater than the surrounding earth. Given the
> gravitational field at a series of locations above the surface find
> the location, orientation and size of the ellipsoid that best fits
> the data.

While in reality this is a very naive geological model and would fail
to describe nearly every real ore deposit, the problem can be used to
investigate evolutionary algorithms. A solution to the problem above
should address the following issues.

* Can the algorithm find the optimal solution?
* Is the performance of the solution satisfactory?
* Out of the large number of EAs and their variants which method
  perfoms the best? Which is suited to this type of problem and why?
* How is the solution implemented? Which programming languages or
  libraries are suited to this type of problem?
* Parallelism should be investigated. Is there a significant
  performance advantage in developing a parallel solution?

Following the completion of the first objective the second objective
is to implement a program that addresses some more advanced issues.

* The simple ellipsoid model has a small number of parameters. Is the
  evolutionary algorithm capable of handling more complicated models
  with hundreds or thousands of parameters? How many parameters can be
  used?
* The problem above is an optimization problem and involves finding
  the best parameters for the model. But what if the model is unknown?
  Are there EAs capable of creating a model that fits the data? If so
  how effective are they?
* In reality there is not a unique solution when given just
  gravtitational and magnetic readings [@Dmod]. Downhole drill samples
  can be used to place constraints on solutions. How easily can these
  added constraints be implemented in the EA?

The expected outcomes of this project are

* A working implementation of EAs in the context of geological
  modelling. This should include a demonstration where the software
  solves a problem and graphically shows the results/progress.
* Justafiable evidence of the perfomance and effectiveness of
  evolutionary methods compared to other techniques. 
* An understanding of EAs and why they are or are not suited to
  geological modelling with all findings recorded in a report.



# Background

## Geological Modelling

Mapping geological features under the earths surface is an important
task not only for academic reasons but also for commercial and
economical reasons. For example resivoir engineers in the mining, oil
and gas industries use geological models as a staring point to create
simulations that guide the development of the resivoir and the
eventual exctraction of material. Methods for producing high quality
geological models are therefore sought after and actively researched.

The geological models can be constructed from various geophysical
sources such as gravitational and magnetic field measurements,
Downhole drill samples and seismic serveys [@Dmod]. Often the field
measurements are collected using airbourne serveying techniques.
Methods such as EAs can produce a large number of solutions but for
each model they produce there needs to be some mechanism for
evaluating their quality. In general this amounts to solving some
PDEs for the gravitational/magnetic fields and then summing to find
the error. For example the gravitational potential (hence the field)
is the solution to Poisson's Equation

$$\nabla^{2}\phi=4\pi G\rho$$

However constructing acurate 3D models from magnetic and gravitational
data alone is not enough. Nettleton stated in 1942 (as quoted in [@Dmod]).

> Unless certain controls other than the gravity and magnetic data are
> available the inherent ambiguities of the physically possible
> distributions of material which can produce the observed effects make
> accurate calculations meaningless even though the geophysical data may
> be of any desired precision.

To reduce the number of solutions that describe the fields correctly
other constraints such as Down-hole drill samples can be used. The
initial problem described in the project outline involves optimizing
an ellipsoid. This type of problem can be classified under discrete
object modelling [@Dmod]. The advantage of using these simple models
is that a number of analytical solutions exist for calculating the
fields. Many simple objects may be combined to represent complex
structures with the total fields found using superposition.


## Evolutionary Algorithms

Evolutionary Algorithms are based off the ideas of natural evolution
established by Darwin. The principle is described in [@ec] as a
stochastic trial-and-error style of problem solving. This does
not mean that the method isn't powerful because one only has to look
at the wide range of species that inhabit our world, each with traits
designed and honed to survive in a range of harsh environments. 

The following points describe the process that governs all EAs.

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
mutation and recombination. These operators provide a source of
*genetic variation* that not only allows for better individuals not
seen in the initial populatioin but also gives the algorithm the
ability to search the entire solution space and avoid the problem of
getting stuck in local mixima.

### Representation of Solutions

How to represent individuals is a very important concept in
evolutionary computing [@ec]. The term *phenotype* is used to refer to
the possible solutions in their most natural representation. Say for
example the problem at hand is to build a decision tree that satifies
a set of inputs and outputs. Obviously a tree-like data structure is a
more natural representation of a solution then say a binary string or
a list of floating point numbers.

Whilst defining a phenotype is important the EAs themselves (mainly
the genetic operators) operate on a problem indepented representation
called the *genotype*. For example the genotype may be a binary string
eventhough the phenotype is a tree-like structure. In some problems
the phenotype/genotype representations might be very different and for
some they might be identical.

For the EA to work there needs to be at most one phenotype
representation for each genotype [@ec]. The process of converting from
phenotype to genotype is referred to as encoding with decoding
meaning the inverse.

### Selection and Genetic Operators

While all EAs follow the general process outlined above they can
differ in a variety of ways, most notable is the structure of the
genotype representation. Because all EAs adhere to the same principles
their is a certain overlap when it comes to implementation. Below is a
list of properties common to EAs [@ec]. For brevity detailed
explinations are left out.

* The ability to produce random individuals
* A method for selecting parents. Some posibilities are
    + Fitness proportional selection also known as roulette wheel
    + selection.
    + Ranking selection
    + Tournament selection
* A set of genetic operators. While these depend on the type
  of EA they usually fall into the following catagories
    + Mutation
        - swaps
        - inverions
    + Recombination
        - one-point crossover
        - two-point crossover
* A method for survivor selection/replacement
    + Age based replacement
    + replacce worst individuals
    + eletism

Once the EA is implemented there is still a number of parameters such
as population size, mutation/recombination rates etc. that must be
chosen before each run.

### Types of EAs

Among the first and most popular EAs invented were Genetic Algorithms
(GAs), introduced by Holland [@holland] in the 1960's [@ec]. In GAs
the structure of the genotype is a fixed length vector/string. The
genotype is usually a vector of binary bits, integers or floating
point values. Because GAs use fixed length vectors they have very good
performance characteristics. But this comes at the cost of not being
able to represent solution of unknown complexity. In other words GAs
are useful for optimization problems where the model is known. But are
limited for modelling problems and machine learning.

Much later in the early 1990's Genetic Programming (GP) was
introduced, with major contributions by Koza [@ec;@koza]. GP uses a
parse-tree structure as its genotype. Each node of the parse-tree
represents a multi-parameter function and each leaf a parameter. This
means the genotype represents some arbitrary program. If the parse
trees are allowed to grow they can evolve to become arbitrarily
complex thus making GP better suited to modelling problems and machine
learning. However this can also be a disadvantage because if a limit
is not placed on the size of the trees the time-space requirements
grow and the program can exhibit bad performance characteristics.

Whilst gentic algorithms and genetic programming are arguably the most
popular evolutionary algorithms others exist such as Gene Expression
Programming [@gep] and Evolutionary Stratergies [@ec]

# Project Plan

## Risk Assesment

\pagebreak <!--this puts refs on seperate page-->



# References
<!-- pandoc handles the refs -->

