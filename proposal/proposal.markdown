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

> Consider an ore deposit which is modelled as by an ellipsiod with a
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
  gravtitational and magnetic readings. Downhole drill samples can be
  used to place constraints on solutions. How easily can these added
  constraints be implemented in the EA?

The expected outcomes of this project are

* A working implementation of EAs in the context of geological
  modelling. This should include a demonstration where the software
  solves a problem and graphically shows the results/progress.
* Justafiable evidence of the perfomance and effectiveness of
  evolutionary methods compared to other techniques. 
* An understanding of EAs and why they are or are not suited to
  geological modelling.
* All findings are to be summarized in a report.

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

This means other sources are needed to validate solutions.

The initial problem described in the project outline involved
optimizing an ellipsoid. This type of problem can be classified under
discrete object modelling [@Dmod]. The advantage of using these simple
models is that a number of analytical solutions exist for calculating
the field. However

## Evolutionary Algorithms

Simply put Evolutionary Algorithms are based off the ideas of natural
evolution established by Darwin. The principle is described in [@ec]
as a stochastic trial-and-error style problem solving process. This does
not mean that the method isn't powerful because one only has to look
at the wide range of species that inhabit our world, each with traits
designed and honed to survive in a range of harsh environments. The
following points describe the process that governs all EAs.

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

### Types of Evolutionary Algorithms

While all EAs follow the general process outlined in the prevoius
section they can differ in a variety of ways most notable is the
method used to represent individuals. One of the first EAs was
invented by Holland in the 1960's and is called genetic algorithm
(GA). Much later in the early 1990's Koza introduced genetic
programming (GP) [@ec]


# Project Plan

## Risk Assesment

\pagebreak <!--this puts refs on seperate page-->

# References
<!-- pandoc handles the refs -->

