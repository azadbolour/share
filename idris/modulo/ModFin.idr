{-
Definition of group operations and proofs of group axioms for integers modulo m.
In this module, integers modulo m are represented as values of type Fin m.
Incomplete. Defined plus modulo m so far.
-}

import Data.Fin

{-
Note. The way Fin is defined in Idris, the successor of a Fin has a different
type parameter: x: Fin k, FS x: Fin (S k). But we need our successors
to have the same Fin type parameters, namely, the modulus m.
To fix the types of successors and predecessors, the functions 'strengthen'
(decrement the Fin type parameter), and 'weaken' (increment the Fin type
parameter) and are used.
-}

||| Fin is sized by its natural number representation.
||| Size is used for defining recursive functions based on decreasing values.
implementation Sized (Fin m) where
  size = finToNat

||| Base zero.
||| Used to define zeros of higher Fin limits.
Z1 : Fin 1
Z1 = FZ

||| The successor of an integer modulo m.
total
succ: Fin m -> Fin m
succ {m = S k} x with (strengthen (FS x))
  succ {m = S k} x | (Left _) = FZ
  succ {m = S k} x | (Right sx) = sx

||| The predecessor of an integer modulo m.
total
pred: Fin m -> Fin m
pred {m = S k} FZ = rewrite plusCommutative 1 k in (shift k Z1)
pred {m = S k} (FS x) = weaken x

{-
For a non-zero value of Fin m, simple recursion is based on the
inductive hypothesis on its predecessor. In order to tell Idris that
doing recursions in this manner is terminating, we need to prove that
the size of FS x is strictly greater than the size of its predecessor
(that is, weaken x, as defined above).

We do this is two steps:

  - prove that weakening does not change size
  - prove that the size of x is strictly less than the size of FS x
-}

||| Weakening a Fin does not change its size (only its limit).
eqWeak : (x : Fin m) -> size (weaken x) = size x
eqWeak FZ = Refl
eqWeak (FS x) = cong {f=S} (eqWeak x)

||| Size of a Fin is strictly less that the size of its successor.
finLtFS : (x : Fin m) -> LT (size x) (size (FS x))
finLtFS x = LTESucc $ lteRefl {n = size x}

||| The modulo predecessor of a non-zero 'Fin m' is smaller in size.
weakenLtFS : (x: Fin m) -> Smaller (weaken x) (FS x)
weakenLtFS x = rewrite eqWeak x in finLtFS x

{-
Modulo addition is defined by induction on the predecessor by using
the general size-based induction pattern implemented in the 'sizeRec'
function:

    sizeRec : Sized a
      =>  (step : (x : a) -> ((y : a) -> Smaller y x -> b) -> b)
          -> (z : a)
          -> b

    sizeRec step z = accRec step z (sizeAccessible z)

Note that the definition of sizeRec uses a strong inductive hypothesis,
which includes the results of the evaluation of all y's smaller than x.
But recursive addition for (FS x) just needs to use the value for the
predecessor of (FS x), that is, for y = weaken x.

Modulo addition is a curried function of its first argument. So the
inductive definition of addition is based on this first argument.
Thus, the 'value' defined by induction is the 'section' of addition
that closes on the first addend: a function that takes the second
addend as its argument and produces the sum, a (Fin m).
-}

||| Recursive definition of modulo addition as a curried function:
||| Fin m -> (Fin m -> Fin m), that takes the first addend, and returns
||| an addition 'section' function taking the second addend as a parameter.
||| The recursive definition is based on induction on the predecessor of
||| first addend.
|||
||| This function is used as the inductive 'step' of the sizeRec pattern
||| for recursion on a sized parameter.
modPlusInductiveStep :
  (first: Fin m)                                                                 -- The first addend.
  -> ((predFirst: Fin m) -> Smaller predFirst first -> (second: Fin m) -> Fin m) -- The strong inductive hypothesis.
  -> (second: Fin m) -> Fin m                                                    -- The 'section' function for the 2nd addend.
modPlusInductiveStep FZ inductiveHypothesis = id
modPlusInductiveStep (FS x) inductiveHypothesis =
  \added => succ (inductiveHypothesis (weaken x) (weakenLtFS x) added)

||| Modulo addition defined inductively by using the generic
||| size-based induction function 'sizeRec'.
total
modPlus: Fin m -> Fin m -> Fin m
modPlus first = sizeRec modPlusInductiveStep first

infix 5 +++

||| A modulo addition operator.
total
(+++) : Fin m -> Fin m -> Fin m
(+++) x y = modPlus x y

-- Tests for modulo plus.

Plus2_2: Fin 5
Plus2_2 = 2 +++ 2

Plus2_2_ok: Plus2_2 = 4
Plus2_2_ok = Refl

Plus2_3: Fin 5
Plus2_3 = 2 +++ 3

Plus2_3_ok: Plus2_3 = 0
Plus2_3_ok = Refl

Plus2_4: Fin 5
Plus2_4 = 2 +++ 4

Plus2_4_ok: Plus2_4 = 1
Plus2_4_ok = Refl

Plus4_2: Fin 5
Plus4_2 = 4 +++ 2

Plus4_2_ok: Plus2_4 = 1
Plus4_2_ok = Refl
