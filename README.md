Buffon machines [![Build Status](https://travis-ci.org/maciej-bendkowski/buffon-machines.svg?branch=master)](https://travis-ci.org/maciej-bendkowski/buffon-machines) [![Hackage](https://img.shields.io/badge/hackage-v1.1.1.0-blue.svg)](https://hackage.haskell.org/package/buffon-machines) [![License](https://img.shields.io/badge/license-BSD--3-orange.svg)](https://tldrlegal.com/license/bsd-3-clause-license-(revised))
---------------

*Buffon machines* is a simple, monadic implementation of Buffon machines [1]
meant for *perfect* simulation of discrete random variables using a discrete
oracle of random bits. Buffon machines are implemented as monadic computations
consuming random bits, provided by a 32-bit buffered oracle. Bit regeneration
and computation composition is handled within the monad itself.

The main purpose of *Buffon machines* is to provide an experimental framework
for discrete random variable generation required in the design and
implementation of various combinatorial samplers, such as analytic samplers
(a.k.a. Boltzmann samplers). In particular, its goal is to provide tools to
*perfectly* simulate discrete distributions using as few random bits as
possible.

The current implementation provides several basic generators discussed in [1].
In particular, it offers perfect generators for geometric, Poisson, and
logarithmic distributions with given rational or real (i.e.  double-precision
floating) parameters, as well as a bit-optimal discrete uniform variable and
Bernoulli generators described in [2]. More involved Buffon machines can be
compiled using the provided combinators.

General, non-uniform discrete variable generation, in the spirit of Knuth and
Yao [3], is also available, cf. [4]. However, it should be noted that the current
implementation does not achieve optimal average bit consumption, except for a
limited number of special cases.

References
----------

 [1] Ph. Flajolet, M. Pelletier, M. Soria : “On Buffon Machines and Numbers”,
     SODA'11 - ACM/SIAM Symposium on Discrete Algorithms, San Francisco, USA,
     pp. 172-183, (Society for Industrial and Applied Mathematics) (2011)

 [2] J. Lumbroso : "Optimal Discrete Uniform Generation
     from Coin Flips, and Applications".

 [3] D. Knuth, A. Yao : "The complexity of nonuniform random number generation",
     in Algorithms and Complexity: New Directions and Recent Results,
     Academic Press, (1976)

 [4] F. Saad, C. Freer, M. Rinard, V. Mansinghka : "Optimal Approximate Sampling
     from Discrete Probability Distributions".
