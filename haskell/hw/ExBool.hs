module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where
    show False = "False"
    ahow True = "True"

instance Enum Bool where

    toEnum 1 = True
    toEnum 0 = False
    toEnum _ = error "no boolean value assigned"

    fromEnum True = 1
    fromEnum False = 0

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False


infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
(||) False False = False
(||) _ _ = True


infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) True True = False
(/|\) _ _ = True

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) False False = True
(\|/) _ _ = False

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) True False = True
(<=/=>) False True = True
(<=/=>) _ _ = False

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a b = a
ifThenElse False a b = b

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) True False = False
(==>) _ _ = True

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) False True = False
(<==) _ _ = True

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) True True = True
(<=>) False False = True
(<=>) _ _ = False

infixr 1 <=>

