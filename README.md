Buffon machines [![Build Status](https://travis-ci.org/maciej-bendkowski/buffon-machines.svg?branch=master)](https://travis-ci.org/maciej-bendkowski/buffon-machines) [![License](https://img.shields.io/badge/license-BSD--3-orange.svg)](https://tldrlegal.com/license/bsd-3-clause-license-(revised))
---------------

*Buffon machines* is a simple, monadic implementation of Buffon machines meant
[1] for *perfect* simulation of discrete random variables using a discrete
oracle of random bits. Buffon machines are implemented as monadic computations
consuming random bits, provided by a 32-bit buffered oracle. Bit regeneration
and computation composition is handled within the monad itself.

The current implementation provides several basic generators discussed in [1].
In particular, it offers perfect generators for geometric, Poisson, and
logarithmic distributions with given rational or real (i.e.  double-precision
floating) parameters, as well as a bit-optimal discrete uniform variable and
Bernoulli generators described in [2]. More involved Buffon machines can be
compiled using the provided combinators.

References
----------

 [1] Ph. Flajolet, M. Pelletier, M. Soria : “On Buffon Machines and Numbers”,
     SODA'11 - ACM/SIAM Symposium on Discrete Algorithms, San Francisco, USA,
     pp. 172-183, (Society for Industrial and Applied Mathematics) (2011)
 
 [2] J. Lumbroso : "Optimal Discrete Uniform Generation
     from Coin Flips, and Applications".
