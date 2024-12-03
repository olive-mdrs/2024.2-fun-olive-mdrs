module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x : xs) = 
	if x == Nothing 
	then catMaybes xs 
	else (x : (catmaybes xs))

fromJust :: Maybe a -> a
fromJust Just a = a
fromJust _ = error "No value"

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ Just b = b

isJust :: Maybe a -> Bool
isJust (Just _) = True
_ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust 

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

justMap :: (a -> Maybe b) -> [a] -> [b]
justMap f [] = []
justMap f (x : xs) =
	    | f x == Just b = (b : justMap f xs)
	    | f x == nothing = justMap f xs

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b f (Just a) = f a
maybe b f Nothing = b

maybeToList :: Maybe a -> [a]
maybeToList Just a = [a]
MaybeToList _ = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
liatToMaybe (x : xs) = Just x

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith _ [] = []
tryToModifyWith [] [a] = [a]
tryToModifyWith (x : xs) (y : ys) =
	  | x == Just f = ((f y) : tryToModifyWith xs ys))
	  | x == Nothing _ = (y : (tryToModifyWith xs ys))

