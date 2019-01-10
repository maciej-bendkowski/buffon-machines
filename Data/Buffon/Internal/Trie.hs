{-|
 Module      : Data.Buffon.Internal.Trie
 Description : Naive tries (digital trees) for boolean strings.
 Copyright   : (c) Maciej Bendkowski, 2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 A naive implementation of tries (digital trees) for boolean strings used as an
intermediate tool during the construction of permutation order types.
 -}
module Data.Buffon.Internal.Trie
    ( -- * Trie data structure.
      Trie(..)
    , empty
    , isEmpty
    , insert
    , bulk
    , search
    ) where

import Data.Maybe

-- | Trie for boolean strings.
data Trie =
    Trie { left  :: Maybe Trie -- ^ positive bits (1).
         , right :: Maybe Trie -- ^ negative bits (0).
         } deriving (Show)

-- | Empty trie.
empty :: Trie
empty =
    Trie { left  = Nothing
         , right = Nothing }

-- | Checks if the given trie is empty or not.
isEmpty :: Trie -> Bool
isEmpty trie =
    isNothing (left trie) && isNothing (right trie)

insert' :: [Bool] -> Maybe Trie -> Maybe Trie
insert' xs Nothing     = Just $ insert xs empty
insert' xs (Just trie) = Just $ insert xs trie

-- | Inserts the given string into a trie.
insert :: [Bool] -> Trie -> Trie
insert [] trie = trie
insert (x : xs) trie =
  if x then trie { left  = insert' xs (left trie) }
       else trie { right = insert' xs (right trie) }

-- | Bulk (sequential) insert.
bulk :: [[Bool]] -> Trie
bulk = foldr insert empty

-- | Checks if the given boolean string
--   is present in the trie or not.
search :: [Bool] -> Trie -> Bool
search [] trie = isEmpty trie
search (x : xs) trie =
  if x then case left trie of
              Nothing -> False
              Just lt -> search xs lt

       else case right trie of
              Nothing -> False
              Just rt -> search xs rt
