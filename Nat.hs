module Nat where

import Prelude
  hiding ((+), (*), (^), (-), (<), (>), (++), quot, min, gcd, lcm, div, max, pred, rem,
  length, elem, sum, product, reverse, enumFromTo, enumTo, take, drop, init, last, minimum, maximum, isPrefixOf)

data Nat = O | S Nat
  deriving (Eq , Show)

data ListNat = Empty | Cons Nat ListNat
    deriving( Eq, Show)

--type ListNat = [Nat]

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
_ < O = False
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

-- MDC --
gcd :: (Nat, Nat) -> Nat
gcd (n, 0) = n
gcd (n, m) = gcd (m, rem (n, m))

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

-- Product --i
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
reverse (Cons n ns) = append n (reverse ns)

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
take :: Nat -> ListNat -> ListNat
take (S m) (Cons n ns) = Cons n (take m ns)
take _ _ = Empty

-- drop --
drop :: Nat -> ListNat -> ListNat
drop O n = n
drop (S m) (Cons n ns) = drop m ns


-- elemIndices --
elemIndices :: Nat -> ListNat -> ListNat
elemIndices _ Empty = Empty
elemIndices m (Cons n ns)
  | n == m    = Cons O (addNat (S O) (elemIndices m ns))
  | otherwise = addNat (S O) (elemIndices m ns)

-- pwAdd --
pwAdd :: ListNat -> ListNat -> ListNat
pwAdd _ Empty = Empty
pwAdd (Cons n ns) (Cons m ms) = Cons (n + m) (pwAdd ns ms)

-- pwMult --
pwMult :: ListNat -> ListNat -> ListNat
pwMult _ Empty = Empty
pwMult (Cons n ns) (Cons m ms) = Cons (n * m) (pwMult ns ms)

-- Is Sorted --
isSorted :: ListNat -> Bool
isSorted (Cons n (Cons m ms)) = (n < m) && isSorted (Cons m ms)
isSorted Empty = True

-- Filter Even --
filterEven :: ListNat -> ListNat
filterEven Empty = Empty
filterEven (Cons n ns)
  | ev n = Cons n (filterEven ns)
  | otherwise = filterEven ns

-- Filter Odd --
filterOdd :: ListNat -> ListNat
filterOdd Empty = Empty
filterOdd (Cons n ns)
  | od n = Cons n (filterOdd ns)
  | otherwise = filterOdd ns

-- minimum --
minimum :: ListNat -> Nat
minimum (Cons n Empty) = n
minimum (Cons n ns) = min n (minimum ns)

-- Maximum --
maximum :: ListNat -> Nat
maximum (Cons n Empty) = n
maximum (Cons n ns) = max n (maximum ns)

-- Is Prefix of --
isPrefixOf :: ListNat -> ListNat -> Bool
isPrefixOf Empty _ = True
isPrefixOf (Cons n ns) (Cons m ms)
  | n == m && isPrefixOf ns ms = True
  | otherwise = False

-- mix --
mix :: ListNat -> ListNat -> ListNat
mix (Cons n ns) (Cons m ms) = Cons n (Cons m (mix ns ms))
mix _ _ = Empty

-- Intersperse --
intersperse :: Nat -> ListNat -> ListNat
intersperse _ Empty = Empty
intersperse n (Cons m ns) = Cons m (Cons n (intersperse n ns))

-- Head --
head :: ListNat -> Nat
head Empty = undefined
head (Cons n ns) = n

-- Tail --
tail :: ListNat -> ListNat
tail Empty = undefined
tail (Cons n ns) = ns

-- Init --
init :: ListNat -> ListNat
init (Cons n Empty) = Empty
init (Cons n ns) = Cons n (init ns)

-- Last --
last :: ListNat -> Nat
last (Cons n Empty) = n
last (Cons n ns) = last ns
