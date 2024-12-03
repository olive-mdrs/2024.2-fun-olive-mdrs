module ExFunctor where

import Prelude hiding ( fmap , (<$) )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const


instance Funktor [] where
    fmap = map

instance Funktor Maybe where
    fmap = mapMaybe

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f Just a = Just f a

instance Funktor Either where
	fmap = mapEither

mapEither :: (a -> b) -> Either a' a -> Either a' b
mapEither f Left e = Left e
mapEither f Right x = Right (f x)

instance Funktor (a,b) where
	fmap = mapPair

mapPair :: (b -> c) -> (a,b) -> (a,c)
mapPair f Pair a b = Pair a (f b)

instance Funktor (a -> b) where
	fmap = ffmap

ffmap :: (b -> c) -> (a -> b) -> (a -> c)
ffmap f g = g . f

-- what about Trees?

-- ...define Functor instances of as many * -> * things as you can think of!

