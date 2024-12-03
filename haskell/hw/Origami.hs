module Origami where

import Prelude hiding
    ( foldl , foldl1 , foldr , foldr1
    , sum , product
    , length
    , concat
    , filter
    , map
    , any , all
    , and , or
    , takeWhile , dropWhile
    )

import qualified Prelude as P

--
-- define the following folds:
--

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a -> a -> a) -> a -> [a] -> a
foldr _ e [] = e
foldr op e (x : xs) = 
	let v = foldr op e xs
	in op x v

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (a -> a -> a) -> a -> [a] -> a
foldl _ e [] = e
foldl op e (x : xs) =
	let v = foldl op e xs
	in op v x

-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [] = error "empty list"
foldr1 op [x,y] = op x y
foldr1 op (x : xs) = op x (foldr1 op xs)

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 _ [] = error "empty list"
folsl1 op [x,y] = op y x
foldl1 op (x : xs) = op (foldl1 op xs) x

--
-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
--
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
--

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ e [] = [e]
scanl f e (x : xs) = ((scanl f e xs) : (foldl f e (x : xs)))

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ e = [e]
scanr f e (x : xs) = ((foldr f e (x : xs)) : (scanr f e xs))

--
-- Define all of the following functions as folds:
--

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

concat :: [[a]] -> [a]
concat = foldr (++) []

any :: (a -> Bool) -> [a] -> Bool
any = map . foldr (||) False

all :: (a -> Bool) -> [a] -> Bool
all = map . foldr (&&) True

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

minimum :: Ord a => [a] -> a
minimum = undefined

maximum :: Ord a => [a] -> a
maximum = undefined

length :: Integral i => [a] -> i
length = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

reverse :: [a] -> [a]
reverse = undefined

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = undefined

-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
semo :: Integral i => [i] -> (i, Maybe i)
semo = undefined

-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: Eq a => [a] -> [a]
remdups = undefined

safeLast :: [a] -> Maybe a
safeLast = undefined

-- dec2int [1,9,9,2] = 1992
dec2int :: Integral i => [i] -> i
dec2int = undefined

