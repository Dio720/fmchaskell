module Nat where
import Prelude
  hiding ((+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem)
import Data.Bits (Bits(xor))

data Nat = O | S Nat
  deriving (Eq , Show)


-- Predecessor --
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- Adição --
(+) :: Nat -> Nat -> Nat
n + O = n
n + (S m) = S (n + m)

-- Subtração --
(-) :: Nat -> Nat -> Nat
n - O = n
O - n = O
n - (S m) = S (n + m)

-- Multiplicação -- +-
(*) :: Nat -> Nat -> Nat
n * O = O
n * S m = (n * m) + n 

-- Potenciação --
(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ (S m) = (n ^ m) * n

-- Dobro --
double :: Nat -> Nat
double O = O
double (S n) = S(S (double n))

-- Fibonacci --
fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = fib (S n) + fib n

-- Mínimo --
min :: Nat -> Nat -> Nat
min n O = O
min O m = O
min (S n) (S m) = S (min n m)

-- Máximo --
max :: Nat -> Nat -> Nat
max n O = n
max O m = m
max (S n) (S m) = S (max n m)

-- Divide --

-- quot