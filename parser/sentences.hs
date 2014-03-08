-- | Enumerating all sentences in a language.
--
module Acrid.Sentences (
  word,
  (//),
  (&)
) where

import Acrid.Common
import Data.Monoid

-- | Enumerates all ways one list of sentences can follow another.
--
-- >>> ["a","b"] & ["x","y"]
-- ["ax","ay","bx","by"]
--
-- >>> word "a" & word "b" == word "ab" 
-- True
--
(&) :: (Monoid a) => [a] -> [a] -> [a]
(&) = carp (mappend)
infixl 4 &


-- | Adds a list of sentences to the language.
--
-- >>> ["a","b"] // ["x","y"]
-- ["a","x","b","y"]
--
(//) :: [a] -> [a] -> [a]
(//) = interleave
infixl 3 //


-- | Takes a sequence of terminals and makes it a sentence.
--
-- >>> word "abc" // word "xyz"
-- ["abc","xyz"]
--
-- >>> "abc" // "xyz"
-- "axbycz"
--
word :: [a] -> [[a]]
word = (:[])
