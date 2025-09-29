{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) O O = True
    (==) _ O = False
    (==) O _ = False
    (==) (S n) (S m) = (==) n m 

instance Ord Nat where

    (<=) O O = True
    (<=) O _ = True
    (<=) _ O = False
    (<=) (S n) (S m) = (<=) n m


    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O n = O
    min n O = O
    min (S n) (S m) = S(min n m) 

    max O n = n
    max n O = n
    max (S n) (S m) = S(max n m) 


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False 

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O   = n
n <+> (S m) = S (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus n O = n
monus O _ = O
monus (S n) (S m) = monus n m

(<->) :: Nat -> Nat -> Nat
(<->) = monus

-- multiplication
times :: Nat -> Nat -> Nat
times n O = O
times n (S m) = n + times n m

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow n O = S O
pow n (S m) = n * pow n m


exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = undefined
O </> _ = O
n </> (S m) = 
  case n - m of
    O -> O
    _ -> S ((</>) (n - S m) (S m))

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O = error "divisão por zero"
n <%> (S m) = n - ((n </> S m) * S m)


-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (n, m) = (n </> m, n <%> m)

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) O O = True
(<|>) O n = False
(<|>) n O = True
(<|>) n m = 
  case m <%> n of
    O -> True
    _ -> False

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n m = (n - m) + (m - n)

(|-|) = dist

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = (S n) * (factorial n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg _ = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O O = undefined
lo _ O = undefined
lo O _ = undefined
lo (S O) (S O) = undefined
lo (S O) _ = undefined
lo _ (S O) = O
lo b a = 
  case a < b of
    False -> S (lo b (a </> b))
    True -> O


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat n = S (toNat (n-1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n

-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = O
      | otherwise = toNat x

