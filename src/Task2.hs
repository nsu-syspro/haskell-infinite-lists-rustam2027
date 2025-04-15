{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where
import Data.Foldable (Foldable(toList))

-- | Infinite stream of elements
data Stream a = Stream a (Stream a)

instance Foldable Stream where
  foldMap f (Stream a s) = f a <> foldMap f s

instance Show a => Show (Stream a) where
  show = show . take 15 . toList

instance Functor Stream where
  fmap f (Stream a s) = Stream (f a) (fmap f s)

filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p (Stream a s) = if p a then Stream a (filterStream p s) else filterStream p s

zipStreamsWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipStreamsWith f (Stream a as) (Stream b bs) = Stream (f a b) (zipStreamsWith f as bs) 

-- | Converts given list into stream
--
-- If the list is finite then it is continued
-- with given value repeated infinitely
--
-- Usage example:
--
-- >>> fromList 0 [1,2,3]
-- [1,2,3,0,0,0,0,0,0,0]
-- >>> fromList undefined [1..]
-- [1,2,3,4,5,6,7,8,9,10]
--
fromList :: a -> [a] -> Stream a
fromList e (x:xs) = Stream x (fromList e xs)
fromList e _ = Stream e (fromList e [])

-- | Builds stream from given seed value by applying given step function
--
-- Step function produces a pair of the next element in stream and updated seed value.
--
-- Usage example:
--
-- >>> unfold (\x -> (x, x-1)) 5
-- [5,4,3,2,1,0,-1,-2,-3,-4]
-- >>> unfold (\x -> (abs x, x-1)) 5
-- [5,4,3,2,1,0,1,2,3,4]
--
unfold :: (b -> (a, b)) -> b -> Stream a
unfold f b = Stream l (unfold f r) where
  (l, r) = f b

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = unfold (\x -> (x, x + 1)) 1

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = unfold (\(x, y) -> (x, (y, x + y))) (0, 1)

-- | Returns infinite stream of prime numbers
--
-- First 10 prime numbers:
--
-- >>> primes
-- [2,3,5,7,11,13,17,19,23,29]
--
primes :: Stream Integer
primes = unfold sieve (fromList undefined [2..])

-- | One step of Sieve of Eratosthenes
-- (to be used with 'unfoldr')
--
-- Returns next prime number from given stream
-- and strikes out all multiples of this prime
-- from the rest of the stream
--
-- Usage example:
--
-- >>> sieve $ fromList 0 [2..]
-- (2,[3,5,7,9,11,13,15,17,19,21])
-- >>> sieve $ snd $ sieve $ fromList 0 [2..]
-- (3,[5,7,11,13,17,19,23,25,29,31])
--
sieve :: Stream Integer -> (Integer, Stream Integer)
sieve (Stream f s) = (f, filterStream (\x -> x `mod` f /= 0) s)
