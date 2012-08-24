## 03/08/12 19:59:01
Okay im going to rewrite the haskell version focusing on

  * configuration via dsl
  * parse the config around in the reader monad
  * make deposit more general/abstract ie Individual/Population
  * log details using the writer monad
  * use random-fu/mwc-random for sampling random variables
  * build monad stack using transformers
  * think about using vector / sequence / something else for speed

##05/08/12 00:00:44
While googling haskell libraries i stumbled upon GEP "Gene Expression
Programming" very cool. After reading more about them i've convinced myself
that my initial approach is probably not the best way to go. After all the
things I'm trying to optimize are the n floating point parameters that define
the deposit so creating complex data structures in haskell along with specific
operations does seem a bit pointless especially considering performance. In
the end all the GA, GEP or GP sees is the fitness function and the parameters
so may as well use the more powerful, well establish and efficient methods in
literature. So this time a rewrite following the GEP-PO method outlined in 
Cândida Ferreira's Book. Still focus on what i've learned so far

  * just do configuration in haskell dsl style
  * parse config in reader monad
  * log output with the reader monad
  * try MonadRandom for stateful random calculations
  * quickcheck should work for GEP specifics
  * if needed use transformers to build monad stack
  * ByteString/Vector where posible maybe even repa
  * think about parallel

