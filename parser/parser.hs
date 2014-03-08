module Acrid.Parser (
  Result,
  label,
  rest,
  Parser,
  (</>),
  parse,
  tok,
  str,
  anyIn,
  complete,
  first,
  (.>),
  (<.),
  eta,
  end,
  module Control.Applicative
) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (stripPrefix)
import Control.Applicative
import Data.Dynamic
import Acrid.Common


-- | Result tuple that a Parser will return a list of.
-- label: The user-defined label associated with the parse result.
--  rest: Terminals remaining after the parse completed.
--
data Result a b = Result { label :: b, rest :: [a] }


-- | Map the Result label from one type to another.
--
instance Functor (Result a) where
  fmap f res = res { label = f $ label res }
--


-- | Show instance for Result.
--
instance (Show a, Show b) => Show (Result a b) where
  show (Result lbl rst) = '{' : show lbl ++ "," ++ show rst ++ "}"
--


-- | Parser type. Takes a list of terminals and returns a list of Result.
--
newtype Parser a b = Parser ([a] -> [Result a b])


-- | Maps the Result label returned by the parser from one type to another.
--
instance Functor (Parser a) where
  fmap f (Parser p) = Parser $ (fmap . fmap) f . p
--


-- | 'a <*> b' is Parser 'a' followed by Parser 'b' as a new Parser 'c'.
--   The Result label of 'c' is the label of 'a' applied to the label of 
--   'b'.
--
-- >>> parse ((,) <$> tok 'a' <*> tok 'b') "ab"
-- [{('a','b'),""}]
--
--
instance Applicative (Parser a) where
  pure a = eta a
  a <*> b = Parser $ \xs -> resume ($) (parse a xs) b 
--


-- | 'a <|> b' is the combined Results of Parser 'a' and Parser 'b' as the
--   new Parser 'c'.
--
-- >>> parse (tok 'a' <|> tok 'b') "ab"
-- [{'a',"b"}]
--
-- >>> parse (tok 'a' <|> tok 'b') "ba"
-- [{'b',"a"}]
--
--
instance Alternative (Parser a) where
  empty = Parser (const [])
  (Parser a) <|> (Parser b) = Parser $ liftA2 interleave a b
--


-- | 'a </> b' is Parser 'a' followed by zero or more repetitions of
--   Parser 'b' as the new Parser 'c'. The label of 'c' is the repeated
--   application of the label of 'b'.
--
-- >>> parse (tok 2 </> (-) <$> tok 1) [2,1,1,1]
-- [{2,[1,1,1]},{-1,[1,1]},{2,[1]},{-1,[]}]
--
--
(</>) :: Parser a b -> Parser a (b -> b) -> Parser a b
a </> b = Parser $ p . parse a
  where
  p [] = []
  p left = interleave left $ p $ resume (flip ($)) left b
infixl 2 </>


-- | Parser composition. Right parser first, left follows.
--
(<.) :: Parser a (b -> c) -> Parser a (d -> b) -> Parser a (d -> c)
a <. b = Parser $ \xs -> resume (flip (.)) (parse b xs) a
infixr 9 <.
--


-- | Parser composition. Left parser first, right follows.
--
(.>) :: Parser a (d -> b) -> Parser a (b -> c) -> Parser a (d -> c)
(.>) = flip (<.)
infixl 9 .>


-- | Parser which always matches and consumes no input.
--
-- >>> parse (eta ()) "zzz"
-- [{(),"zzz"}]
--
-- >>> parse (eta ()) ""
-- [{(),""}]
--
--
eta :: b -> Parser a b
eta b = Parser $ pure . Result b


-- | Parser which only matches when there is no remaining input.
--
-- >>> parse (end ()) "zzz"
-- []
--
-- >>> parse (end ()) ""
-- [{(),""}]
--
--
end :: b -> Parser a b
end b = Parser $ \xs -> if null xs then [Result b []] else []


-- | Resumes parsing from a list of Results, using 'f' to combine the
--   labels.
--
resume :: (b -> c -> d) -> [Result a b] -> Parser a c -> [Result a d]
resume f left p = foldr interleave [] . fmap right $ left
  where
  right (Result lbl rst) = (fmap . fmap) (f lbl) $ parse p rst
--


-- | Returns the parsing function from the Parser type.
--
parse :: Parser a b -> [a] -> [Result a b]
parse (Parser p) = p


-- | Creates a Parser that will only match the given terminal.
--
-- >>> parse (tok 'a') "za"
-- []
--
-- >>> parse (tok 'a') "aa"
-- [{'a',"a"}]
--
--
tok :: (Eq a) => a -> Parser a a
tok t = Parser p
  where
  p [] = []
  p (x:xs) = if t == x then [Result t xs] else []
--


-- | Creates a Parser that will only match the given sequence of terminals.
--
-- >>> parse (str "abc") "abz"
-- []
--
-- >>> parse (str "abc") "abc"
-- [{"abc",""}]
--
--
str :: (Eq a) => [a] -> Parser a [a]
str s = Parser $ maybe [] (pure . Result s) . stripPrefix s


-- | Creates a Parser that will match any one terminal in the given list.
--
-- >>> parse (anyIn ['a'..'z']) "123"
-- []
--
-- >>> parse (anyIn ['a'..'z']) "abc"
-- [{'a',"bc"}]
--
--
anyIn :: (Eq a) => [a] -> Parser a a
anyIn yss = Parser p
  where
  p [] = []
  p (x:xs) = if x `elem` yss then [Result x xs] else []  
--


-- | Filters parse Results to only those which consumed the entire input.
--
-- >>> complete $ parse (tok 'a') "aaa"
-- ""
--
-- >>> complete $ parse (tok 'a') "a"
-- "a"
--
--
complete :: [Result a b] -> [b]
complete = fmap label . filter (null . rest)


-- | Returns the first complete parse Result, or Nothing.
--
first :: [Result a b] -> Maybe b
first = listToMaybe . complete
