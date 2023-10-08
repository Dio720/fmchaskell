module Nat where

import Prelude
  hiding ((+), (*), (^), (-), (<), (>), (++), quot, min, gcd, lcm, div, max, pred, rem, length, elem, sum, product, reverse, enumFromTo, enumTo)
import Distribution.CabalSpecVersion (HasCommonStanzas(NoCommonStanzas))
import Data.Sequence (ViewR(EmptyR))


data Nat = O | S Nat
  deriving (Eq , Show)

data ListNat = Empty | Cons Nat ListNat
    deriving( Eq, Show)

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
n - (S m) = n - m

-- Multiplicação -- +-
(*) :: Nat -> Nat -> Nat
n * O = O
n * S m = (n * m) + n

-- Potenciação --
(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ (S m) = (n ^ m) * n


-- Menor que --
(<) :: Nat -> Nat -> Bool
O < m = True
(S n) < O = False
(S n) < (S m) = n < m

-- Dobro --
double :: Nat -> Nat
double O = O
double (S n) = S (S (double n))

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

-- Divisâo --
div :: (Nat, Nat) -> (Nat, Nat)
div (_, O) = undefined
div (n, m)
  | n < m     = (O, m)
  | otherwise = let (q', r') = div (n - m, n)
                 in (S q', r')

-- Quociente -- 
quot :: (Nat,Nat) -> Nat
quot (n, m) = fst (div (n, m))

-- Resto -- 
rem :: (Nat, Nat) -> Nat
rem (n, m) = snd (div (n, m))

{-
-- MDC --
gcd :: (Nat, Nat) -> Nat
gcd (n, m)
  | m == O    = n
  | otherwise = gcd (m, rem (n, m))
-}

-- Even --
ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S (S n)) = ev n

-- Odd --
od :: Nat -> Bool
od O = False
od (S O) = True
od (S (S n)) = od n

-- IsZero --
isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- LISTAS --

-- Length --
length :: ListNat -> Nat
length Empty = O
length (Cons _ ns) = S (length ns)

-- Elem --
elem :: Nat -> ListNat -> Bool
elem _ Empty = False
elem m (Cons n ns) = n == m || elem m ns

-- Sum --
sum :: ListNat -> Nat
sum Empty = O
sum (Cons n ns) = n + sum ns

-- Product --
product :: ListNat -> Nat
product Empty = S O
product (Cons n ns) = n * product ns

-- Concat --
(++) :: ListNat -> ListNat -> ListNat
Empty ++ ms = ms
(Cons n ns) ++ ms = Cons n (ns ++ ms)

-- Append --
append :: Nat -> ListNat -> ListNat
append n Empty = Cons n Empty
append n (Cons m ms) = Cons m (append n ms)

-- Reverse --
reverse :: ListNat -> ListNat
reverse Empty = Empty
reverse (Cons n ns) = reverse ns ++ Cons n Empty


-- All even --
allEven :: ListNat -> Bool
allEven Empty = True
allEven (Cons n ns) = ev n && allEven ns

-- Any zero --
anyZero :: ListNat -> Bool
anyZero Empty = False
anyZero (Cons n ns) = isZero n || anyZero ns

-- Add Nat --
addNat :: Nat -> ListNat -> ListNat
addNat _ Empty = Empty
addNat n (Cons m ns) = Cons (n + m) (addNat n ns)

-- Mul Nat --
multNat :: Nat -> ListNat -> ListNat
multNat _ Empty = Empty
multNat n (Cons m ns) = Cons (n * m) (multNat n ns)

-- Exp Nat --
expNat :: Nat -> ListNat -> ListNat
expNat _ Empty = Empty
expNat n (Cons m ns) = Cons (n ^ m) (expNat n ns)

-- Enum From To --
enumFromTo :: Nat -> Nat -> ListNat
enumFromTo n m
  | n < m     = Cons n (enumFromTo (S n) m)
  | n == m    = Cons m Empty
  | otherwise = Empty

-- Enum To --
enumTo :: Nat -> ListNat
enumTo = enumFromTo O

-- take --
take :: Nat -> ListNat
