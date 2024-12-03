{-# LANGUAGE GADTs #-}

module Prova1 where

import Prelude hiding
    ( map , filter , curry , uncurry
    , odd , min , plus , prod , times , len
    , take , dropWhile , tails , concat , repeat , (++) , any , replicate
    , zip , pairs , subseqs , inits , zipWith , countDown
    , zip , zipWith
    )

---------------- A (35 pts) ----------------


-- define: Nat
data Nat :: type where
	O :: Nat
	S :: Nat -> Nat

-- define: List
data List a :: type -> type where
	[] :: List a
	(:) :: a -> List a -> a

-- define: map
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = (f x) : map f xs

-- define: filter
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs) =
	if f x
	then (x : filter f xs)
	else filter f xs

-- define: fold
fold :: (a -> a -> a) -> a -> [a] -> a
fold _ e [] = []
fold op e (x : xs) = op x (fold op e xs)

-- define: curry
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f a b = f (a,b))

-- define: uncurry
uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (a,b) = f a b

---------------- B (32 pts) ----------------
-- define 4 of the following functions

-- 4 pts each
odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n

min :: Nat -> Nat -> Nat
min n O = n
min O _ = O
min (S n) (S m) = min n m

plus :: Nat -> Nat -> Nat
plus n O = n
plus n (S m) = S (n m)

prod :: List Nat -> Nat
prod [] = 1
prod (x : xs) = times x (prod xs)

times :: Nat -> Nat -> Nat
times n O = O
times n (S m) = plus (times n m) n

len :: List a -> Nat
len [] = O
len (x : xs) = S (len xs)

-- 6 pts each
take      :: Nat -> List a -> List a
take _ [] = []
take O (x : xs) = []
take n (x : xs) = (x : take (n-1) xs)

dropWhile :: (a -> Bool) -> List a -> List a
dropwhile _ [] = []
dropwhile p (x : xs) = 
	if p x
	then dropwhile p xs
	else (x : xs)

tails     :: List a -> List (List a)
tails [] = [[]]
tails (x : xs) = (xs : tails xs)

concat    :: List (List a) -> List a          
concat [[]] = []
concat (l : ls) = l ++ (concat ls)

repeat    :: a -> List a
repeat a = (a : repeat a)

(++)      :: List a -> List a -> List a   
(++) xs [] = xs
(++) [] xs = xs
(++) (x : xs) ys = (x : ((++) xs ys))

any       :: (a -> Bool) -> List a -> Bool
any _ [] = False
any p (x : xs) = p x || any p xs

replicate :: Nat -> a -> List a           
replicate 0 _ = []
replicate n a = (a : replicate (n-1) a)

-- 8 pts each
zip       :: List a -> List b -> List (a,b)
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = ((x,y) : zip xs ys)

pairs     :: List a -> List (a,a)   
pairs [] = []
pairs (x : (y : ys)) = ((x,y) : pairs ys)

subseqs   :: List a -> List (List a)
subseqs [] = [[]]
subseqs (x : xs) = subseqs xs ++ (map (:x) subseqs xs)

auxinit :: List a -> List a
auxinit [] = error "empty list"
auxinit [x] = []
auxinit (x : xs) = (x : auxinit xs)

inits     :: List a -> List (List a)
inits [] = [[]]
inits (x : xs) = ((x : auxinit xs) : inits xs)

zipWith   :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = ((f x y) : zipWith f xs ys)

countDown :: Nat -> List Nat
countDown O = [O]
countDown (S n) = ((S n) : countDown n)

---------------- C (12 pts) ----------------
-- define zip in terms of zipWith
auxpair :: a -> b -> (a,b)1
auxpair a b = (a,b)

zip' = zipwith auxpair

-- define zipWith in terms of zip
zipWith' f xs ys = map (uncurry f) (zip xs ys)  

---------------- D (21 pts) ----------------

-- D1: define ArEx
data ArEx where
	Lit :: Int -> ArEx
	(+) :: ArEx -> ArEx -> ArEx
	(*) :: ArEx -> ArEx -> ArEx
	(-) :: ArEx -> ArEx

-- D2: define eval
eval :: ArEx -> Int
eval Lit n = n
eval (+) x y = eval x + eval y
eval (*) x y = eval x * eval y
eval (-) x = - (eval x)

-- D3: define height
height :: ArEx -> Nat
height Lit n = O
height (-) x = S (height x)
height _ x y = S (auxmax (height x) (height y))

auxmax :: Nat -> Nat -> Nat
auxmax n O = n
auxmax O n = n
auxmax (S n) (S m) = S (auxmax n m)
