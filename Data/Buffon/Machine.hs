{-|
 Module      : Data.Buffon.Machine
 Description : Buffon Machines and related distribution generators.
 Copyright   : (c) Maciej Bendkowski, 2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

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
Yao [3], is also available. However, it should be noted that the current
implementation does not achieve optimal average bit consumption, except for a
limited number of special cases.

References:

[1] Ph. Flajolet, M. Pelletier, M. Soria : “On Buffon Machines and Numbers”,
    SODA'11 - ACM/SIAM Symposium on Discrete Algorithms, San Francisco, USA,
    pp. 172-183, (Society for Industrial and Applied Mathematics) (2011)

[2] J. Lumbroso : "Optimal Discrete Uniform Generation
    from Coin Flips, and Applications".

[3] D. Knuth, A. Yao : "The complexity of nonuniform random number generation",
    in Algorithms and Complexity: New Directions and Recent Results,
    Academic Press, (1976)
 -}
{-# LANGUAGE BangPatterns, DeriveLift #-}
module Data.Buffon.Machine
    ( -- * Buffon machines and related utilities.
      Rand(..), empty, init
    , BuffonMachine, runRIO
    , histogram, histogramIO
    , samples, samplesIO, samplesIO'

    -- * Random variables.
    , Bern, Discrete
    , toDiscrete

    -- * Coin flips.
    , flip, flip'

    -- * Bernoulli variable generators.
    , rational, real

    -- * Buffon machine combinators.
    , repeat, cond, neg
    , (/\), (\/), square
    , mean, even
    , exp, recipLog

    -- * Discrete variable generators.
    , geometric, geometricReal, geometricRational, vonNeumann
    , poisson, generalPoisson, poissonReal, poissonRational
    , logarithmic, logarithmicReal, logarithmicRational

    -- * Uniform variable generator.
    , uniform

    -- * Non-uniform variable generator.
    , DecisionTree(..), decisionTree
    , unveil, maxFlips, minFlips
    , avgFlips, choice
    ) where

import Prelude hiding (flip, init, recip,
                        repeat, even, exp)

import qualified Prelude as P

import Control.Monad
import Control.Monad.State.Strict

import Data.Bits
import Data.Word (Word32)
import qualified Data.List as L

import Numeric (floatToDigits)

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as S

import System.Random

import Language.Haskell.TH.Syntax (Lift(..))

-- | 32-bit buffered random bit generator (RBG).
data Rand g =
    Rand { buffer  :: !Word32 -- ^ Generator buffer.
         , counter :: !Int    -- ^ Number of consumed buffer bits.
         , oracle  :: !g      -- ^ Random bit oracle.
         }

-- | Checks if the given RBG is empty or not.
--   In other words, if a buffer refill is required.
empty :: Rand g -> Bool
empty rng = counter rng == 32
{-# INLINE empty #-}

-- | A fresh RBG.
init :: RandomGen g => g -> Rand g
init g = case random g of
          (x, g') -> Rand { buffer  = x
                          , counter = 0
                          , oracle  = g' }
{-# INLINE init #-}

-- | Computations consuming random bits using RBGs.
--   Note that the implementation is essentially a State monad,
--   passing RNG throughout its computations.
type BuffonMachine g a = State (Rand g) a

-- | Runs the given Buffon machine within the IO monad
--    using StdGen as its random bit oracle.
runRIO :: BuffonMachine StdGen a -> IO a
runRIO m = evalState m . init <$> getStdGen

samples' :: RandomGen g
        => BuffonMachine g a -> Int -> [a]
        -> BuffonMachine g [a]

samples' _ 0 xs = return xs
samples' m !n xs = do
    x <- m
    samples' m (pred n) (x : xs)

-- | Using the given discrete variable
--   (Buffon machine) outputs n random samples.
samples :: RandomGen g
        => BuffonMachine g a -> Int -> BuffonMachine g [a]

samples m n = samples' m n []

-- | Runs 'samples' within the IO monad.
samplesIO :: BuffonMachine StdGen a -> Int -> IO [a]
samplesIO m n = runRIO (samples m n)

-- | A space efficient variant of 'samplesIO'.
samplesIO' :: BuffonMachine StdGen a -> Int -> IO [a]
samplesIO' m n = samplesIO'' m n []

samplesIO'' :: BuffonMachine StdGen a -> Int -> [a] -> IO [a]
samplesIO'' _ 0 xs = return xs
samplesIO'' m !n xs = do
    x <- runRIO m
    samplesIO'' m (pred n) (x : xs)

-- | Computes a histogram of the given discrete random variable.
--   The variable (Buffon machine) is evaluated n times and the data
--   is collected in form of a multiset occurrence list.
histogram :: RandomGen g
          => Discrete g -> Int
          -> BuffonMachine g [(Int, S.Occur)]

histogram m n = do
    ms <- histogram' m n S.empty
    return $ S.toOccurList ms

histogram' :: RandomGen g
           => Discrete g -> Int -> MultiSet Int
           -> BuffonMachine g (MultiSet Int)

histogram' _ 0 s = return s
histogram' m !n s = do
    x <- m
    let s' = S.insert x s
    histogram' m (pred n) s'

-- | A 'histogram' variant within the IO monad.
histogramIO :: BuffonMachine StdGen Int -> Int ->  IO ()
histogramIO m n = runRIO (histogram m n) >>= print

-- | Bernoulli variables.
type Bern g = BuffonMachine g Bool

-- | General discrete variables.
type Discrete g = BuffonMachine g Int

-- | Lifts a Bernoulli variable to a discrete one.
toDiscrete :: Bern g -> Discrete g
toDiscrete m = do
    b <- m
    return $ if b then 1
                  else 0

oracle' :: RandomGen g => Rand g -> Rand g
oracle' rng
    | empty rng = init (oracle rng)
    | otherwise = rng
{-# INLINE oracle' #-}

-- | Random coin flip. Note that the implementation
--   handles the regeneration of the RBG, see 'Rand'.
flip :: RandomGen g => Bern g
flip = do
    modify' oracle'
    rng <- get
    put $ rng { counter = succ (counter rng) }
    return $ testBit (buffer rng) (counter rng) -- test respective bit.
{-# INLINE flip #-}

-- | Fair variant of flip. Implements the following, standard trick.
--   Use 'flip' twice and continue if and only if both coin
--   flips yield the same bits. Although such a trick yields a
--   truly fair coin flip, it should be noted that the standard
--   'flip' is reasonably fair (and at the same time more efficient).
flip' :: RandomGen g => Bern g
flip' = do
    b0 <- flip
    b1 <- flip
    case (b0, b1) of
      (False, True) -> return False
      (True, False) -> return True
      _             -> flip'

-- | Evaluates the given Bernoulli variable n times
--   and returns a list of resulting values.
repeat :: RandomGen g
       => Int -> Bern g -> BuffonMachine g [Bool]

repeat 0 _ = return []
repeat !n m = do
    b  <- m
    bs <- repeat (pred n) m
    return (b : bs)

-- | Given parameters a < b, both positive, returns a Bernoulli
--   variable with rational parameter λ = a/b. Note: Implements
--   the algorithm 'Bernoulli' described by J. Lumbroso.
rational :: RandomGen g => Int -> Int -> Bern g
rational a b = do
    let v = 2*a
    heads <- flip
    if v >= b then
        if heads then rational (v - b) b
                 else return True
    else
        if heads then rational v b
                 else return False

-- | Binary expansions.
type Bin = [Bool]

toBool :: Int -> Bool
toBool 0 = False
toBool 1 = True
toBool _ = error "Absurd case"

binExpansion' :: [Int] -> Int -> [Bool]
binExpansion' bs 0 = map toBool bs
binExpansion' bs !n = False : binExpansion' bs (succ n)

binExpansion :: Double -> [Bool]
binExpansion x = binExpansion' bs n
    where (bs, n) = floatToDigits 2 x

real' :: RandomGen g => Bin -> Bern g
real' [] = error "Absurd case"
real' (b : bs) = do
    heads <- flip'
    if heads then real' bs
             else return b

-- | Bernoulli variable with the given double-precision parameter.
--   Note: the given parameter has to lie within 0 and 1 as otherwise
--   the outcome is undefined.
real :: RandomGen g => Double -> Bern g
real x = real' (binExpansion x)

-- | Conditional if-then-else combinator.
cond :: Bern g                  -- ^ 'if' condition ...
     -> BuffonMachine g a       -- ^ ... 'then' expression ...
     -> BuffonMachine g a       -- ^ ... 'else' expression.
     -> BuffonMachine g a

cond p f g = do
    pv <- p
    if pv then f
          else g

-- | Negation combinator.
neg :: Bern g -> Bern g
neg p = cond p (return False) (return True)

-- | Conjunction combinator.
(/\) :: Bern g -> Bern g -> Bern g
(/\) p q = cond p q (return False)

-- | Disjunction combinator.
(\/) :: Bern g -> Bern g -> Bern g
(\/) p = cond p (return True)

-- | Squaring combinator.
square :: Bern g -> Bern g
square p = p /\ p

-- | Mean combinator.
mean :: RandomGen g
     => BuffonMachine g a
     -> BuffonMachine g a
     -> BuffonMachine g a

mean = cond flip

-- | Even-parity combinator.
even :: RandomGen g => Bern g -> Bern g
even p =
    cond (neg p) (return True)
        (cond (neg p) (return False)
            (even p))

-- | Given a Bernoulli variable with parameter λ, realises
--   a geometric variable with the same parameter λ.
geometric :: Bern g -> Discrete g
geometric p = geometric' p 0

geometric' :: Bern g -> Int -> Discrete g
geometric' p n = cond (neg p)
                      (return n)
                      (geometric' p $ succ n)

-- | A variant of geometric using the 'real' Bernoulli generator.
geometricReal :: RandomGen g => Double -> Discrete g
geometricReal x = geometric (real x)

-- | A variant of geometric using the 'rational' Bernoulli generator.
geometricRational :: RandomGen g => Int -> Int -> Discrete g
geometricRational p q = geometric (rational p q)

-- | Tries for random binary strings.
data RandTrie = Leaf Int
          | Node (Maybe RandTrie)
                 (Maybe RandTrie)

orderType :: RandTrie -> [Int]
orderType t = leaves t []

type DiffList = [Int] -> [Int]

leaves :: RandTrie -> DiffList
leaves (Leaf n)     = ([n] ++)
leaves (Node lt rt) =
    case (lt, rt) of
      (Just lt', Nothing)  -> leaves lt'
      (Nothing, Just rt')  -> leaves rt'
      (Just lt', Just rt') -> leaves lt' . leaves rt'
      _                    -> ([] ++)

root :: Maybe RandTrie
root = Just (Node Nothing Nothing)

-- | Inserts a random, infinite bit string into the given trie.
--   Note that the bit strings are computed (and updated) lazily.
insertR :: RandomGen g
       => Maybe RandTrie
       -> Int
       -> BuffonMachine g (Maybe RandTrie)

insertR (Just (Leaf k)) n = do
    node' <- insertR root k
    insertR node' n

insertR (Just (Node lt rt)) n = do
    c <- flip
    if c then do
        lt' <- insertR lt n
        return $ Just (Node lt' rt)
    else do
        rt' <- insertR rt n
        return $ Just (Node lt rt')

insertR Nothing n = return (Just $ Leaf n)

nats :: [Int]
nats = [0..]

-- | Constructs a trie for n random boolean strings.
trie :: RandomGen g
     => Int
     -> BuffonMachine g (Maybe RandTrie)

trie n = foldM insertR root (take n nats)

-- | General, von Neumann generation scheme.
vonNeumann :: RandomGen g
           => ([Int] -> Bool) -- ^ Permutation tester.
           -> Discrete g      -- ^ Geometric variable (Buffon machine).
           -> Discrete g

vonNeumann test p = do
    n <- p
    t <- trie n
    case t of
      Nothing -> error "Absurd case"
      Just t' -> if test (orderType t') then return n
                                        else vonNeumann test p

-- | Sorted permutations.
sorted :: [Int] -> Bool
sorted []           = True
sorted [_]          = True
sorted (n : m : ns) =
    n <= m && sorted (m : ns)

-- | Cyclic permutations.
--   Note: This function does not test if a given list
--   represents a cycle. Instead, it checks if the initial
--   element is the list's maximal element. Both classes of
--   permutations are in fact isomorphic. In other words,
--   there exist (n-1)! cycles of length n as well as
--   (n-1)! lists of n elements such that their initial
--   elements are also their maximal elements.
cyclic :: [Int] -> Bool
cyclic [] = False
cyclic (n : ns) =
    all (n >) ns

-- | Given a geometric variable with parameter λ
--   realises a Poisson variable with the same parameter λ.
--   Note: the parameter λ has to lie within (0,1).
poisson :: RandomGen g => Discrete g -> Discrete g
poisson = vonNeumann sorted

-- | Given a geometric variable with parameter λ
--   realises a Poisson variable with the same parameter λ.
--   Note: the current implementation is linear in the parameter λ.
generalPoisson :: RandomGen g => Double -> Discrete g
generalPoisson p =
    let (n, x) = properFraction p
     in do
         m <- poissonN 0.5 (2 * n) -- integer part
         if 0 < x && x < 1 then do
                             x' <- poissonReal x
                             return (m + x')
                           else return m

poissonN :: RandomGen g => Double -> Int -> Discrete g
poissonN p n = do
    let m = geometric (real p)
    xs <- samples (poisson m) n
    return $ L.foldl' (+) 0 xs

-- | Poisson distribution with the given double-precision parameter.
poissonReal :: RandomGen g => Double -> Discrete g
poissonReal x = poisson (geometric $ real x)

-- | Given two positive and relatively prime integers 'p' and 'q'
--   realises a Poisson variable with the paramter p/q.
poissonRational :: RandomGen g => Int -> Int -> Discrete g
poissonRational p q = poisson (geometric $ rational p q)

-- | Given a geometric variable with parameter λ
--   realises a logarithmic variable with the same parameter λ.
logarithmic :: RandomGen g => Discrete g -> Discrete g
logarithmic = vonNeumann cyclic

-- | Logarithmic distribution with the given double-precision parameter.
logarithmicReal :: RandomGen g => Double -> Discrete g
logarithmicReal x = logarithmic (geometric $ real x)

-- | Given two positive and relatively prime integers 'p' and 'q'
--   realises a logarithmic variable with the paramter p/q.
logarithmicRational :: RandomGen g => Int -> Int -> Discrete g
logarithmicRational p q = logarithmic (geometric $ rational p q)

-- | Given a Bernoulli variable with parameter λ
--   realises a Bernoulli variable with the parameter exp(-λ).
exp :: RandomGen g => Bern g -> Bern g
exp = poisson' 0

-- | Given a Bernoulli variable with parameter λ
--   realises a Bernoulli variable with the parameter λ/(log(1-λ)^{-1}).
recipLog :: RandomGen g => Bern g -> Bern g
recipLog = poisson' 1

poisson' :: RandomGen g => Int -> Bern g -> Bern g
poisson' k m = do
    n <- poisson (geometric m)
    return (n == k)

-- | Uniform random variable with support {0,1,...,n-1}.
--   Note: 'uniform' is an implementation of the FastDiceRoller
--   algorithm described by J. Lumbroso.
uniform :: RandomGen g => Int -> Discrete g
uniform n = uniform' n 1 0

uniform' :: RandomGen g => Int -> Int -> Int -> Discrete g
uniform' n !v !c = do
    b <- flip
    let v' = 2 * v
    let c' = 2 * c + fromEnum b
    if n <= v' then
        if c' < n then return c'
                  else uniform' n (v' - n) (c' - n)
               else uniform' n v' c'

-- | Given a set of probabilities p_1 up to p_{n-1}, 'layout'
--   aligns them on the [0,1) real line segment, assigning
--   the remaining 1 - sum_{i=1,...,n-1} to the final event.
--   So, for instance, 'layout' applied to [0.25,0.25,0.25]
--   lays out four, equally likely events on the segment [0,1).
layout :: [Double] -> [Bin]
layout xs = ys ++ [n]
    where xs' = L.scanl1 (+) xs
          ys  = map binExpansion xs'
          n   = L.repeat True

-- | Decision trees.
data DecisionTree a = Decision a
                    | Toss (DecisionTree a)
                           (DecisionTree a)
                            deriving (Show,Lift)

-- | General, depth-aware toll function.
toll :: Num a => (Int -> a -> a -> a)
              -> Int -> DecisionTree b -> a

toll _ _ (Decision _) = 0
toll f !d (Toss lt rt) = f d lt' rt'
    where lt' = toll f (succ d) lt
          rt' = toll f (succ d) rt

-- | Computes the maximal number of flips required
--   to make a definite decision for the given tree.
maxFlips :: DecisionTree a -> Int
maxFlips = toll (\_ a b -> succ $ max a b) 0

-- | Computes the minimal number of flips required
--   to make a definite decision for the given tree.
minFlips :: DecisionTree a -> Int
minFlips = toll (\_ a b -> succ $ min a b) 0

-- | Computes the average-case number of flips required
--   to make a definite decision for the given tree.
avgFlips :: DecisionTree a -> Double
avgFlips = toll (\d a b -> P.recip (2 ^ d) + a + b) 0

-- | Returns a string representation of a suitably
--   truncated variant of the given decision tree up
--   to the given depth parameter.
unveil :: Show a => Int -> DecisionTree a -> String
unveil _ (Decision a) = show a
unveil 0 (Toss _ _) = "..."
unveil n (Toss lt rt) = "(" ++ lt' ++ " | " ++ rt' ++ ")"
    where lt' = unveil (pred n) lt
          rt' = unveil (pred n) rt

-- | Determines if the given segment is a regular cut segment.
isCut :: (a, Bin) -> Bool
isCut (_, True : _ : _) = True
isCut _                 = False

-- | Computes a decition tree for the given set of probabilities
--   corresponding to successive outcomes 0,1,...,n-1. Note: the outcome
--   decision tree is not guaranteed to be optimal, in the sense that
--   it minimises the average-case bit consumption. Also, the final
--   probability corresponding to the outcome n is computed
--   automatically, so that it holds p_1 + ... + p_n = 1.
decisionTree :: [Double] -> DecisionTree Int
decisionTree ps = decisionTree' (zip nats ps')
    where ps' = layout ps

decisionTree' :: [(Int, Bin)] -> DecisionTree Int
decisionTree' []       = error "Absurd case"
decisionTree' [(n, _)] = Decision n
decisionTree' ps = Toss lt' rt'
    where (lt, rt) = splits ps
          lt'      = decisionTree' (shave lt)
          rt'      = decisionTree' (shave rt)

splits :: [(a, Bin)] -> ([(a, Bin)], [(a, Bin)])
splits ps =
    case splits' [] ps of
      -- p starts at .1 hence should not split.
      (p, lt @ ((_, [True]) : _), rt) -> (reverse lt, p : rt)

      -- p starts at 0. hence has to split.
      (p, lt, rt)      -> (reverse (p : lt), p : rt)

splits' :: [(a, Bin)] -> [(a, Bin)] -> ((a, Bin), [(a, Bin)], [(a, Bin)])
splits' xs [p] = (p, xs, [])
splits' xs (p : ps)
  | isCut p   = (p, xs, ps)
  | otherwise = splits' (p : xs) ps

splits' _ _ = error "Absurd case"

shave :: [(Int, Bin)] -> [(Int,Bin)]
shave [] = []
shave (p : ps) =
    case p of
      (n, [True]) -> (n, L.repeat True) : shave ps -- scale p to one.
      (n, _ : bs) -> (n, bs) : shave ps            -- multiply by 2.
      _           -> error "Absurd case"

-- | Draws a discrete variable according
--   to the given decision tree.
choice :: RandomGen g
       => DecisionTree a -> BuffonMachine g a

choice !x = do
    heads <- flip
    choice' heads x

{-# SPECIALISE choice ::
        DecisionTree Int -> BuffonMachine StdGen Int #-}

choice' :: RandomGen g
        =>  Bool -> DecisionTree a -> BuffonMachine g a

choice' _ (Decision n) = return n
choice' True (Toss _ rt) = do
    heads <- flip
    choice' heads rt

choice' False (Toss lt _) = do
    heads <- flip
    choice' heads lt

{-# SPECIALISE choice' ::
        Bool -> DecisionTree Int -> BuffonMachine StdGen Int #-}
