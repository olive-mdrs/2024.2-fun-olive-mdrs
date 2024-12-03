{-# LANGUAGE GADTs #-}

module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "empty list"
head (x : xs) = x

tail :: [a] -> [a]
tail [] = error "empty list"
tail (x : xs) = xs

null :: [a] -> Bool
null [] = True
null _ = True

length :: Integral i => [a] -> i
length [] = 0
length (x : xs) = 1 + legth xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = (++) (reverse xs) [x]

(++) :: [a] -> [a] -> [a]
(++) xs [] = xs
(++) (x : xs) ys = (x : ((++) xs ys))

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y : ys) = (y : (snoc x ys))

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm??)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error"empty list"
minimum [x] = x
minimum (x : xs) =
	if x > minimum xs
	then minimum xs
	else x

maximum :: Ord a => [a] -> a
maximum [] = error "empty list"
maximum [x] = x
maximum (x : xs) =
	if x > maximum xs
	then x
	else maximum xs

take :: Nat -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x : xs) = (x : (take (n-1) xs))

drop :: Nat -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (x : xs) = drop (n-1) xs

takeWhile :: (a-> Bool) -> [a] -> [a]
takeWhile _ [] => []
takeWhile p (x : xs) =
	if p x
	then (x : takeWhile p xs)
	else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] => []
dropWhile p (x : xs) =
	if p x
	then dropWhile p xs
	else (x : xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (xs : tails xs)

init :: [a] -> [a]
init [] = error "empty list"
init [x] = [] 
init (x : xs) = (x : init xs)

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = ((x : init xs) : inits xs)

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) = subsequences xs ++ (map (x:) subsequences xs)

any :: (a -> Bool) -> [a] -> Bool
any p [] = False
any p (x : xs) = p x || any f xs

all :: (a -> Bool) -> [a] -> Bool
all p  [] = True
all p (x : xs) = p x && all f xs

and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs

or :: [Bool] => Bool
or [] = False
or (x : xs) = x || or xs

concat :: [[a]] -> [a]
concat [[]] -> []
concat (l : ls) = l ++ (concat ls)

-- elem using the funciton 'any' above
elem :: a -> [a] -> Bool
elem a = any (==a)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem':: a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs) =
	if a == x
	then True
	else elem' a xs

(!!) :: [a] -> Nat -> a
(!!) [] _ = error "empty list"
(!!) (x : xs) 0 = x
(!!) (x : xs) n = (!!) xs (n-1)

filter :: (a -> Bool) -> [a] -> [a]
filter p [] -> []
filter p (x : xs) =
	if p x
	then (x : filter p xs)
	else filter p xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = (f x : map f xs)

-- cycle

repeat :: a -> [a]
repeat a = (a : repeat a)

replicate :: Nat -> a -> [a]
replicate 0 _ = []
replicate n a = (a : replicate (n-1) a)

isPrefixOf :: [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf xs ys =
	if xs == ys
	then True
	else isPrefixOf xs (init ys)

-- isInfixOf :: [a] -> [a] -> Bool

isSuffixOf :: [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf xs (y : ys) =
	if xs == (y : ys)
	then True
	else isSuffixOf xs ys

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = ((x,y) : zip xs ys)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = (f x y : zipWith f xs ys)

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
