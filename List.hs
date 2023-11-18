{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}

module List where
import Prelude hiding (drop, take, zip, dropWhile, takeWhile, (^), (*), (+), reverse, fold, map, (<=), (>=), compare, replicate, filter, all, any, take)

import Nat ( ListNat, Nat(..), (+), pwMult, product )

replicate :: Nat -> a -> [a]
replicate O _ = []
replicate (S n) x = x : replicate n x

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs)
  | f x       = x : filter f xs
  | otherwise = filter f xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x:xs) = f x && all f xs

any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs) = f x || any f xs

pw :: (a -> b -> c) -> [a] -> [b] -> [c]
pw _ [] _ = []
pw _ _ [] = []
pw f (x:xs) (y:ys) = f x y : pw f xs ys

fold :: (a -> a -> a) -> a -> [a] -> a
fold _ x [] = x
fold f x (y:ys) = f y (fold f x ys)

take :: Nat -> [a] -> [a]
take O _ = []
take _ [] = []
take (S n) (x:xs) = x : take n xs

drop :: Nat -> [a] -> [a]
drop O xs = xs 
drop _ [] = []
drop (S n) (x : xs) = drop n xs

