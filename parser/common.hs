-- | Functions in common between Acrid parser modules.
--
module Acrid.Common (
  carp,
  carpl,
  interleave,
  inf
) where

-- | Cartesian product of two lists, using 'f' to combine the pairs.
--
-- This algorithm does not get "stuck" when fed two infinite lists. To
-- demonstrate, a stuck algorithm is compared with 'carp'.
--
-- >>> [(x,y) | y <- [1..], x <- [10..]]
-- [(10,1),(11,1),(12,1),(13,1),(14,1),(15,1),(16,1),(17,1),(18,1) ...
--
-- >>> carp (,) [10..] [1..]
-- [(10,1),(10,2),(11,1),(10,3),(11,2),(12,1),(10,4),(11,3),(12,2) ...
--
-- As seen, the list comprehension is stuck on 'y=1', whereas 'carp'
-- does not have this problem. The output of 'carp' for integers can be
-- mimicked by this list comprehension instead:
--
-- >>> [(x,s-x) | s <- [1..], x <- [10..s-1]]
-- [(10,1),(10,2),(11,1),(10,3),(11,2),(12,1),(10,4),(11,3),(12,2) ...
--
-- An example of 'carp' on non-integers:
--
-- >>> carp (,) [[1.1],[3.2]] "xy"
-- [([1.1],'x'),([1.1],'y'),([3.2],'x'),([3.2],'y')]
--
-- A property of 'carp' is that it evenly works outwards from the start
-- of both lists. This can be seen by the following example.
--
-- >> carp (+) [1..] [1..]
-- [2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,8,8,9,9,9,9,9 ...
--
--
carp :: (a -> b -> c) -> [a] -> [b] -> [c]
carp f [] _ = []
carp f _ [] = []
carp f (x:xs) (y:ys) = topRight [x] xs [y] ys
  where
  topRight [] [] _ _ = []
  topRight xL [] yL [] = zipWith f xL yL ++ topRight (tail xL) [] yL []
  topRight xL [] yL (y:yR) = zipWith f xL yL ++ topRight xL [] (y:yL) yR
  topRight xL (x:xR) yL [] = zipWith f xL yL ++ topRight (tail xL ++ [x]) xR yL []
  topRight xL (x:xR) yL (y:yR) = zipWith f xL yL ++ topRight (xL++[x]) xR (y:yL) yR
--


-- | Cartesian product of two lists, using 'f' to combine the pairs.
--
-- This algorithm shares the same property of 'carp' in that it does not
-- get "stuck" on infinite lists. Refer to the 'carp' documentation for
-- clarification.
--
-- Unlike 'carp', 'carpl' traverses the cartesian product differently,
-- exploring the left list exponentially more than the right. The purpose
-- is to demonstrate a cartesian product definition using 'interleave'.
--
--
carpl :: (a -> b -> c) -> [a] -> [b] -> [c]
carpl f xss = foldr interleave [] . fmap (\y -> fmap (flip f $ y) xss)


-- | Merges the elements of two lists in an alternating fashion.
--
-- This algorithm is designed to merge the elements of two infinite lists
-- without getting "stuck". To demonstrate, a "stuck" algorithm is
-- compared with 'interleave'.
--
-- >>> [1..] ++ [10..]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23 ...
--
-- >>> interleave [1..] [10..]
-- [1,10,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19,11,20 ...
--
-- As seen, the list concatenation operator '++' will put [10..] on the
-- end of [1..], but since [1..] has no end, we will never see any elements
-- from [10..]. 'interleave' solves this problem by repeatedly choosing one
-- element from [1..] then one element from [10..].
--
-- Alternate is defined as non-strict on its right argument. This enables
-- recursive definitions such as the following.
--
-- >>> let abc = interleave "abc" abc in abc
-- "aabacbacbacbacbacbacbacbacbacbacbacbacbacbacbacbacbac ...
--
-- Note that we could have defined interleave to be strict on its right
-- argument and simply used 'inf' to relax the strictness when needed.
-- However, this makes 'carpl' cumbersome to define; loosely, 'inf' does
-- not store "this is an infinite list" on the list itself, so when the
-- length of the list is used to determine the multiplicity of another
-- behaviour, 'inf' does not make it knowable that the other behaviour will
-- occur infinitely.
--
--
interleave :: [a] -> [a] -> [a]
interleave [] yss = yss
interleave (x:xs) yss = x : r
  where
  r = case yss of
    [] -> xs
    (y:ys) -> y : interleave xs ys
--



-- | Enforces that a list is infinite.
--
-- Essentially, this is a hint that a list is guaranteed to be infinite.
-- In practice, it can be used to make a function with a strict list
-- pattern match non-strict.
--
-- > test [] = Nothing
-- > test (x:xs) = Just "hello world"
--
-- >>> test . inf $ []
-- Just "hello world"
--
-- >>> length . take 5 . inf $ undefined
-- 5
--
-- A practical use is to break recursive dependency. These examples use
-- functions from this module.
--
-- >>> let abc = interleave "abc" (inf abc) in abc
-- "aabacbacbacbacbacbacbacbacbacbacbacbacbacbacbacbacbacbacbacbac ...
--
-- >>> let abc = carp (:) "abc" (inf abc) in map (take 3) abc
-- ["aaa","aaa","baa","aba","baa","caa","aab","bba","caa","aba" ...
--
--
inf :: [a] -> [a]
inf ~(x:xs) = x : inf xs
