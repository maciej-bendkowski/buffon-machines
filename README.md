Buffon machines
---------------

*Buffon machines* is a simple, monadic implementation of Buffon machines meant
[1] for *perfect* simulation of discrete random variables using a discrete
oracle of random bits. Buffon machines are implemented as monadic computations
consuming random bits, provided by a 32-bit buffered oracle. Bit regeneration
and computation composition is handled within the monad itself.

The current implementation provides several basic generators discussed within
[1]. In particular, it offers perfect generators for Bernoulli, geometric,
Poisson, and logarithmic distributions with given rational or real (i.e.
double-precision floating) parameters.

Finally, it is possible to compile more involved Buffon machines using the
provided combinator functions.

References
==========

 [1] Ph. Flajolet, M. Pelletier, M. Soria : “On Buffon Machines and Numbers”,
     SODA'11 - ACM/SIAM Symposium on Discrete Algorithms, San Francisco, USA,
     pp. 172-183, (Society for Industrial and Applied Mathematics) (2011)
