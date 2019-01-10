{-|
 Module      : Data.Buffon.Machine
 Description : Buffon Machines and related distribution generators.
 Copyright   : (c) Maciej Bendkowski, 2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

The Data.Buffon.Machine module provides a simple, monadic implementation of
Buffon machines [1] meant for *perfect* simulation of discrete random variables
using a discrete oracle of random bits. Buffon machines are implemented as
monadic computations consuming random bits, provided by a 32-bit buffered
oracle.  Bit regeneration and computation composition is handled within the
monad itself.

The current implementation provides several basic generators discussed within
[1]. In particular, it offers perfect generators for Bernoulli, geometric,
Poisson, and logarithmic distributions with given rational or real (i.e.
double-precision floating) parameters.

Finally, it is possible to compile more involved Buffon machines using the
provided combinator functions.


 [1] Ph. Flajolet, M. Pelletier, M. Soria : “On Buffon Machines and Numbers”,
     SODA'11 - ACM/SIAM Symposium on Discrete Algorithms, San Francisco, USA,
     pp. 172-183, (Society for Industrial and Applied Mathematics) (2011)
 -}
{-# LANGUAGE TupleSections, BangPatterns  #-}
module Data.Buffon.Machine
    ( -- * Buffon machines and related utilities.
      Rand(..), empty, init
    , BuffonMachine(..), runRIO
    , histogram, histogramIO
    , samples, samplesIO

    -- * Random variables.
    , Bern, Discrete
    , toDiscrete

    -- * Coin flips.
    , flip, flip'

    -- * Bernoulli variable generators.
    , dyadic, rational, real

    -- * Buffon machine combinators.
    , repeat, cond, neg
    , (/\), (\/), square
    , mean, even
    , exp, recipLog

    -- * Discrete variable generators.
    , geometric, geometricReal, geometricRational, vonNeumann
    , poisson, generalPoisson, poissonReal, poissonRational
    , logarithmic, logarithmicReal, logarithmicRational
    ) where

import Prelude hiding (flip, init, recip,
                        repeat, even, exp)

import Control.Monad

import Data.Bits
import Data.Word (Word32)
import Data.List (foldl')

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as S

import System.Random

import Numeric (floatToDigits)

import Data.Buffon.Internal.Trie (Trie)
import qualified Data.Buffon.Internal.Trie as T

-- | 32-bit buffered random bit generator (RBG).
data Rand g =
    Rand { buffer  :: Word32 -- ^ Generator buffer.
         , counter :: Int    -- ^ Number of consumed buffer bits.
         , oracle  :: g      -- ^ Random bit oracle.
         }

-- | Checks if the given RBG is empty or not.
--   In other words, if a buffer refill is required.
empty :: Rand g -> Bool
empty rng = counter rng == 32

-- | A fresh RBG.
init :: RandomGen g => g -> Rand g
init g = let (x, g') = random g
             in Rand { buffer  = x
                     , counter = 0
                     , oracle  = g' }

-- | Computations consuming random bits using RBGs.
--   Note that the implementation is essentially a State monad,
--   passing RNG throughout its computations.
newtype BuffonMachine g a =
    BuffonMachine { runR :: Rand g -> (a, Rand g) }

instance Functor (BuffonMachine g) where
    fmap = liftM

instance Applicative (BuffonMachine g) where
    pure  = return
    (<*>) = ap

instance Monad (BuffonMachine g) where
    return x = BuffonMachine (x,)
    (BuffonMachine f) >>= h =
        BuffonMachine $ \rng ->
            let (x, rng') = f rng
             in runR (h x) rng'

-- | Runs the given Buffon machine within the IO monad
--    using StdGen as its random bit oracle.
runRIO :: BuffonMachine StdGen a -> IO a
runRIO m = fst . runR m . init <$> getStdGen

samples' :: RandomGen g
        => Discrete g -> Int -> [Int]
        -> BuffonMachine g [Int]

samples' _ 0 xs = return xs
samples' m !n xs = do
    x <- m
    samples' m (pred n) (x : xs)

-- | Using the given discrete variable (Buffon machine) outputs n random samples.
samples :: RandomGen g => Discrete g -> Int -> BuffonMachine g [Int]
samples m n = samples' m n []

-- | Runs 'samples' within the IO monad.
samplesIO :: Discrete StdGen -> Int -> IO [Int]
samplesIO m n = runRIO (samples m n)

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

mkFlip :: Rand g -> (Bool, Rand g)
mkFlip rng =
    (testBit (buffer rng) (counter rng), -- test the respective bit.
        rng { counter = succ (counter rng) })

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

-- | Random coin flip. Note that the implementation
--   handles the regeneration of the RBG, see 'Rand'.
flip :: RandomGen g => Bern g
flip = BuffonMachine $ \rng ->
    mkFlip $ if empty rng then init (oracle rng)
                          else rng

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

-- | Generates all 2^n boolean strings of length n.
genStream :: Int -> [[Bool]]
genStream 0 = [[]]
genStream !n =  map (False :) (genStream $ pred n)
             ++ map (True :) (genStream $ pred n)

-- | Evaluates the given Bernoulli variable n times
--   and returns a list of resulting values.
repeat :: RandomGen g
       => Int -> Bern g -> BuffonMachine g [Bool]

repeat 0 _ = return []
repeat !n m = do
    b  <- m
    bs <- repeat (pred n) m
    return (b : bs)

-- | Bernoulli variable machine with dyadic parameter λ = s/(2^t).
dyadic :: RandomGen g => Int -> Int -> Bern g
dyadic s t = do
    let ps = take s (genStream t)
    bs <- repeat t flip
    return $ bs `elem` ps

take2 :: Int -> Int -> Int -> ([[Bool]], [[Bool]])
take2 t n m = (ys, ys')
    where (ys, xs) = splitAt n (genStream t)
          ys' = take m xs

rational'' :: RandomGen g => Int -> Trie -> Trie -> BuffonMachine g Bool
rational'' t sx fx = do
    bs <- repeat t flip
    if bs `T.search` sx then return True
                        else if bs `T.search` fx then rational'' t sx fx
                                                 else return False

-- | Bernoulli variable with parameter λ = x/(2^t - y).
--   Note, it must hold a < b < 2^t. Otherwise, the result is undefined.
rational' :: RandomGen g => Int -> Int -> Int -> Bern g
rational' x y t = rational'' t (T.bulk sx) (T.bulk fx)
    where (sx, fx) = take2 t x y

isPower :: Int -> Bool
isPower n = popCount n == 1

succShift :: Int -> Int -> Int
succShift 0 k = k
succShift !n k = succShift (shiftR n 1) (succ k)

nextPower :: Int -> Int
nextPower n
  | isPower n = succ $ succShift n 0 -- note: next *greater* power
  | otherwise = succShift n 0

-- | Given parameters a and b, both positive and relatively prime,
--   returns a Bernoulli variable with rational parameter λ = a/b.
--   Note: 'rational' should not be used for more complicated rationals
--   as its rather ineffective. Alternatively, consider using 'real'.
rational :: RandomGen g => Int -> Int -> Bern g
rational a b = rational' a b' t
    where t  = nextPower b
          b' = shiftL 1 t - b

-- | Binary expansions.
type Bin = [Bool]

toBool :: Int -> Bool
toBool 0 = False
toBool 1 = True
toBool _ = error "Absurd case"

binExpansion' :: [Int] -> Int -> Bin
binExpansion' bs 0 = map toBool bs
binExpansion' bs !n = False : binExpansion' bs (succ n)

binExpansion :: Double -> Bin
binExpansion x = binExpansion' bs n
    where (bs, n) = floatToDigits 2 x

real' :: RandomGen g => Bin -> Bern g
real' [] = error "Absurd case"
real' (b : bs) = do
    c <- flip
    if c == b then real' bs
              else if not c && b then return True
                                 else return False

infinity :: Bool -> [Bool]
infinity x = x : infinity x

-- | Bernoulli variable with the given double-precision parameter.
--   Note: the given parameter has to lie within 0 and 1 as otherwise
--   the outcome is undefined.
real :: RandomGen g => Double -> Bern g
real x = real' (binExpansion x ++ infinity False)

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
    return $ foldl' (+) 0 xs

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
