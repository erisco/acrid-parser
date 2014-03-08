-- | Enumerating all syntax trees in a language.
--
module Acrid.Trees (
  Trees,
  leaf,
  tinf
) where

import Acrid.Common
import Control.Applicative


-- | List of syntax trees.
--
newtype Trees a = Trees [a]
  deriving Show
--


-- | Map a list of trees of one type to a list of trees of another.
--
instance Functor Trees where
  fmap f (Trees xs) = Trees (fmap f xs)
--


-- | The cartesian product of two tree lists.
--
-- Refer to documentation for 'carp'.
--
-- >>> Tree [(,)'a',(,)'b'] <*> Tree "xy"
-- Tree [('a','x'),('a','y'),('b','x'),('b','y')]
--
instance Applicative Trees where
  pure a = Trees [a]
  (Trees xs) <*> (Trees ys) = Trees (carp ($) xs ys)
--


-- | Merge two lists of trees.
--
-- Refer to documentation for 'interleave'.
--
-- >>> Tree "ab" <|> Tree "xy"
-- Tree "axby"
--
instance Alternative Trees where
  empty = Trees []
  (Trees xs) <|> (Trees ys) = Trees (interleave xs ys)
--


-- | Alias for 'pure' in Trees Applicative.
--
-- >>> leaf 'a'
-- Trees "a"
--
leaf :: a -> Trees a
leaf = pure


-- | We can't use 'inf' on Trees because Trees hides the underlying
--   list from outsiders. Therefore, we invite 'inf' onto the backstage.
--
tinf :: Trees a -> Trees a
tinf (Trees xs) = Trees $ inf xs

