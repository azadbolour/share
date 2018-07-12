
-- TODO. Distinguish beween private, export, and public export.
-- For now making everything public export to reduce friction in getting started.

||| Modular arithmetic for natural numbers.
module ModularNat

-- Note. Using globals in an attempt to simplify the setup for getting started.
-- TODO. Once proofs work, refactor to remove globals.

||| The modulus to use for modular artithmetic.
||| TODO. Modulus should not be a global in the final cut!
public export
modulus: Nat
modulus = 5

||| Proof that modulus is positive.
public export
modulusPositive : 1 `LTE` ModularNat.modulus
modulusPositive = LTESucc LTEZero

||| LTModulus - proposition/type depicting that a natural number is
||| less than the modulus.
|||
||| We can prove (LTModulus n) for a number n less than the modulus by using
||| the library function lteAddRight: lteAddRight : (n : Nat) -> LTE n (n + m).
||| The proof is: lteAddRight (S n): LTModulus n, where m is inferred
||| to be (modulus - n).
public export
LtModulus : (num: Nat) -> Type
LtModulus num = S num `LTE` ModularNat.modulus

||| Decision procedure for proving a natural number is less than the modulus.
public export
isLtModulus : (num: Nat) -> Dec (LtModulus num)
isLtModulus num = isLTE (S num) modulus

{-
Natural numbers less that the modulus represent integers modulo the modulus.
This leads to a simple representation of modular integers as a dependent pair
that includes a natural number and a proof that it is less than the modulus.

    (n: Nat ** LTModulus n)

For a given modulus, there are 'modulus' specific type of this form and
each contains a single element.

I will call this abstraction a 'modulo dependent pair'.

Values of modulo dependent pairs form a cyclic sequence where the successor of
(modulo - 1) is zero, and the predecessor of zero is (modulo - 1), and otherwise
successor and predecessor are the corresponding ones for natural numbers.

Unfortunately, I ran into issues attempting to name this abstraction and
to use it as a whole in function parameters. In some cases, rather than
this dependent pair as a function argument, I had to split it up into
two separate arguments.
-}

||| Representation of modular zero.
public export
zModulo : (n ** LtModulus n)
zModulo = (Z ** modulusPositive)

||| Low level implementation of the successor function.
||| The input modulo number is represented by splitting the modulo dependent
||| pair into two seperate parameters.
|||
||| Ideally this should be inlined into the function succ (see below).
||| But there were issues with that that I could not solve.
public export
total
succ' : (num: Nat) -> (bounded: LtModulus num) -> (next: Nat ** LtModulus next)
succ' num bounded with (isLtModulus (S num))
  succ' num _ | (Yes snLtModulus) = (S num ** snLtModulus)
  succ' _ _ | (No _) = zModulo

||| The successor function for modulo dependent pairs.
||| Counterpart of S for naturals.
|||
||| To implement it I had to delegate to an auxiliary function in which
||| the input pair is split into its compoenents.
|||
||| Note that in the signature, the parameter y looks like an independent
||| parameter. But in fact it is identified by Idris with the return
||| from the definition. I have run into cases where Idris does not perform
||| this identification automatically and reports a type error.
public export
succ : (x: Nat ** LtModulus x) -> (y: Nat ** LtModulus y)
succ (x ** pf) = succ' x pf

||| More explicit low-level interface for the successor function.
||| Implementing succ in terms of this interface solves the reduction issues.
||| succ mx = succ'' (fst mx) (snd mx) (isLtModulus (S (fst mx)))
||| But the reduced types of the propositions get quite complicated.
public export
total
succ'' : (n: Nat) -> (nLtModulus: LtModulus n) -> Dec (LtModulus (S n)) -> (next: Nat ** LtModulus next)
succ'' n nLtModulus (Yes snLtModulus) = (S n ** snLtModulus)
succ'' n nLtModulus (No contra) = zModulo

||| Test successor of 2.
succ2 : (n ** S n `LTE` ModularNat.modulus)
succ2 = succ (2 ** lteAddRight 3)

||| Test successor of 4 (should be 0 for modulus = 5).
succ4 : (n ** S n `LTE` ModularNat.modulus)
succ4 = succ (4 ** lteAddRight 5)

||| Low-level implementation of the predecessor function.
public export
total
pred' : (num: Nat) -> (bounded: LtModulus num) -> (prev: Nat ** LtModulus prev)
pred' Z _ = (modulus - 1 ** lteRefl)
pred' (S n) snLtModulus = (n ** lteSuccLeft snLtModulus)

||| The predecessor of a modulo dependent pair. Inverse of the successor.
public export
total
pred : (x: Nat ** LtModulus x) -> (y: Nat ** LtModulus y)
pred (x ** pf) = pred' x pf

propSuccPredInverse : succ (ModularNat.pred mx) = mx
propSuccPredInverse = ?propSuccPredInverse_rhs -- not reduced

propPredSuccInverse : ModularNat.pred (succ mx) = mx
propPredSuccInverse = ?propPredSuccInverse_rhs -- not reduced

||| Low level implementation of addition for modulo arithmetic.
||| Uses an interface in which the dependent pair representations of the
||| two modular numbers are split into their components.
|||
||| TODO. This should not be necessary.
public export
total
plusModulo' :
  (x: Nat) -> (xLtModulus: LtModulus x) ->
  (y: Nat) -> (yLtModulus: LtModulus y) ->
  (added: Nat ** LtModulus added)
plusModulo' Z _ y yLtModulus = (y ** yLtModulus)
plusModulo' (S k) xLtModulus y yLtModulus =
  let succY = succ' y yLtModulus
      kLtModulus = lteSuccLeft xLtModulus
  in plusModulo' k kLtModulus (fst succY) (snd succY)

||| Test modular addition.
plusModulo_2_4 : (n ** LtModulus n)
plusModulo_2_4 = plusModulo' 2 (lteAddRight 3) 4 (lteAddRight 5)

||| Addition for modular numbers represented as modulo dependent pairs.
|||
||| To get the implementation to type check, I had to delegate to a version
||| of plus in which the modulo dependent pairs are split into their
||| respective components.
public export
total
(+) : (x: Nat ** LtModulus x) ->
      (y: Nat ** LtModulus y) ->
      (added: Nat ** LtModulus added)
(+) (x ** xLtModulus) (y ** yLtModulus) = plusModulo' x xLtModulus y yLtModulus

||| The inverse of a modular integer for modular addition.
public export
total
inverseModulo : (n ** LtModulus n) -> (m ** LtModulus m)
inverseModulo ModularNat.zModulo = ModularNat.zModulo
inverseModulo (S n ** snLtModulus) =
  let nLtModulus = lteSuccLeft snLtModulus
      inverseN = inverseModulo (n ** nLtModulus)
  in pred' (fst inverseN) (snd inverseN)

||| Test inverse of 4.
inverse4 : Nat
inverse4 = fst $ inverseModulo ((4 ** lteAddRight 5))

{-
Propositions for modular arithmetic using modulo dependent pair representation.
-}

||| This is just a trivial test.
propSucc' : (mx: (x: Nat ** LtModulus x)) -> succ mx = succ' (fst mx) (snd mx)
propSucc' (x ** pf) = Refl -- This use of succ is reduced.

||| Successor can be factored out of an addition to the left - low-level interface.
propLeftSuccPlus' :
  (x: Nat) -> (xLtModulus: LtModulus x) ->
  (y: Nat) -> (yLtModulus: LtModulus y) ->
  (succ (x ** xLtModulus)) + (y ** yLtModulus) = succ ((x ** xLtModulus) + (y ** yLtModulus))
propLeftSuccPlus' mx my = ?propLeftSuccPlus'_1 -- reduced - complicated

||| Succesor can be factored of an addition to the right - low-leve interface.
propRightSuccPlus' :
  (x: Nat) -> (xLtModulus: LtModulus x) ->
  (y: Nat) -> (yLtModulus: LtModulus y) ->
  (x ** xLtModulus) + succ (y ** yLtModulus) = succ ((x ** xLtModulus) + (y ** yLtModulus))
propRightSuccPlus' x xLtModulus y yLtModulus = ?propRightSuccPlus'_rhs -- reduced - complicated

propRightSuccPlus :
  (mx: (x: Nat ** LtModulus x)) ->
  (my: (y: Nat ** LtModulus y)) ->
  mx + succ my = succ (mx + my)
propRightSuccPlus mx my = ?propRightSuccPlus_rhs -- not reduced

propLeftSuccPlus :
  (mx: (x: Nat ** LtModulus x)) ->
  (my: (y: Nat ** LtModulus y)) ->
  (succ mx) + my = succ (mx + my)
propLeftSuccPlus mx my = ?propLeftSuccPlus_rhs -- not reduced

propPlusModuloAssociative :
  (mx: (x: Nat ** LtModulus x)) ->
  (my: (y: Nat ** LtModulus y)) ->
  (mz: (z: Nat ** LtModulus z)) ->
  mx + (my + mz) = (mx + my) + mz

||| Low-level proof that zero is the right identity for modular addition.
public export
total
propPlusModuloRightId' : (x: Nat) -> (xLtModulus: LtModulus x) ->
  plusModulo' x xLtModulus 0 (LTESucc LTEZero) = (x ** xLtModulus)
propPlusModuloRightId' Z (LTESucc LTEZero) = Refl
propPlusModuloRightId' (S k) xLtModulus = ?propPlusModuloRightId'_1

||| Zero is the right identity for modular addition (using modulo dependent
||| pair representation). Implementation is delegated to a corresponding
||| function in which the modulo dependent pair is split up in the interface.
public export
total
propPlusModuloRightId : (mx: (x: Nat ** LtModulus x)) ->
  (plusModulo' (fst mx) (snd mx) 0 (LTESucc LTEZero)) = mx
propPlusModuloRightId (x ** xLtModulus) = propPlusModuloRightId' x xLtModulus
