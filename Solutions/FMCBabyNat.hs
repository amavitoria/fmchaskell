module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
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

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero 0 = S 0
isZero _ = 0

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred 0 = 0
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat
even 0 = S 0
even (S 0) = 0
even (S (S n)) = even n

odd :: Nat -> Nat
odd 0 = 0
odd (S 0) = S 0
odd (S (S n)) = odd n

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus n 0 = n
monus 0 _ = 0
monus (S n) (S m) = monus n m

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
n * 0 = 0
n * (S m) = n + (n * m)

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
n ^ 0 = S 0
n ^ (S m) = n * (n ^ m)

-- decide: infix? ? ^

-- quotient
(/) :: Nat -> Nat -> Nat
(/) = undefined

-- remainder
(%) :: Nat -> Nat -> Nat
(%) = undefined

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) = undefined

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff n m = (n -* m) + (m -* n)

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial 0 = S 0
factorial (S n) = (S n) * (factorial n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg 0 = 0
sg _ = S 0

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined

