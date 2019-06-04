
-- modular numbers as depedendent pairs
module DPairMod

import PropositionalEquality
import UtilProperties

%default total
%access public export

-- ss, zn, ltSucToLte, minusLte, ltSucToLte, lteNeToLt
-- TODO. how to import specific names from a module?

-- TODO. Use ss zn for LTESucc LTEZero where possible to declutter.


{- Representation of modular numbers as dependent pairs. The first
element of the dependent pair is the natural number value of the
modular number. The second is a proof that the first value is less
than the modulus.

"k + 1" is used as the modulus, since there are no modular numbers for
modulus zero.

For consistency, all functions and data structures are parameterized
by k, that is, 1 less than the modulus. But the modulus is used for
variable names: Mod5_1 : Mod (pred 5) ; Mod5_1 = (1 ** lt5)  -}

Mod : Nat -> Type
Mod k = (x : Nat ** (LT x (S k)))

-- the natural number corresponding to a mudulo number
naturalM : {k : Nat} -> Mod k -> Nat
naturalM (x ** _) = x

-- modular zero
zeroM : {k : Nat} -> Mod k
zeroM {k} = (Z ** LTESucc LTEZero)

-- modular zero for use in types without qualification
ZeroM : {k : Nat} -> Mod k
ZeroM = zeroM

Mod2_1 : Mod (pred 2) ; Mod2_1 = (1 ** ss $ ss zn)
Mod3_1 : Mod (pred 3) ; Mod3_1 = (1 ** ss $ ss zn)

Mod5_1 : Mod (pred 5) ; Mod5_1 = (1 ** ss $ ss zn)
Mod5_2 : Mod (pred 5) ; Mod5_2 = (2 ** ss $ ss $ ss zn)
Mod5_3 : Mod (pred 5) ; Mod5_3 = (3 ** ss $ ss $ ss $ ss zn)
Mod5_4 : Mod (pred 5) ; Mod5_4 = (4 ** ss $ ss $ ss $ ss $ ss zn)

-- successor for modular numbers less than the maximum
incM : {k : Nat} -> (mod : Mod k) -> (LT (naturalM mod) k) -> (Mod k)
incM (x ** _) ltk = (S x ** ss ltk)

-- successor with Dec (x = k)
sucMInternal : {k : Nat} -> (mod : Mod k) -> Dec (naturalM mod = k) -> (Mod k)
sucMInternal (x ** _) (Yes _) = zeroM
sucMInternal {k} (x ** bounded) (No ne) = 
  let ltek = ltSucToLte x k bounded
      ltk = lteNeToLt x k ltek ne
  in incM {k} (x ** bounded) ltk
 
-- modular successor 
sucM : {k : Nat} -> Mod k -> Mod k
sucM {k} mod = sucMInternal {k} mod (decEq (naturalM mod) k)

-- sample successors

eq1 : sucM (sucM Mod5_1) = Mod5_3 ; eq1 = Refl
eq2 : sucM Mod5_4 = zeroM {k = 4}                    ; eq2 = Refl
-- implicit k for sucM is inferred from the k for zeroM - but not the other way around!? 
eq3 : sucM (zeroM {k = 4}) = Mod5_1                  ; eq3 = Refl

-- repeated modular succession - $^ is repeated application
sucMN : {k : Nat} -> (n : Nat) -> Mod k -> Mod k
sucMN {k} n = sucM $^ n

-- sample repeated successions
eq4 : sucMN 3 Mod5_1 = Mod5_4 ; eq4 = Refl
eq5 : sucMN 5 (zeroM {k = 4}) = zeroM                               ; eq5 = Refl
eq6 : sucMN 0 Mod2_1 = Mod2_1 ; eq6 = Refl

-- sample cycling behavior
eq7 : sucMN 12 (zeroM {k = 4}) = sucMN 0 Mod5_2 ; eq7 = Refl

-- sample cycling behavior for edge case of modulo 2 numbers
eq8 : sucMN 1 Mod2_1 = zeroM {k = 1}             ; eq8 = Refl
eq9 : sucMN 2 Mod2_1 = Mod2_1 ; eq9 = Refl
eq10 : sucMN 9 Mod2_1 = zeroM {k = 1}            ; eq10 = Refl

-- succession applied n times to modulo zero
sucMNZero : {k : Nat} -> (n : Nat) -> Mod k
sucMNZero {k} n = sucMN n (zeroM {k})

-- sample progressive succession - see below for proofs of the exemplified successions
sz0 : sucMNZero {k = 4} 0 = zeroM {k = 4}             ; sz0 = Refl
sz1 : sucMNZero {k = 4} 1 = Mod5_1 ; sz1 = Refl
sz2 : sucMNZero {k = 4} 2 = Mod5_2 ; sz2 = Refl
sz3 : sucMNZero {k = 4} 3 = Mod5_3 ; sz3 = Refl
sz4 : sucMNZero {k = 4} 4 = Mod5_4 ; sz4 = Refl
sz5 : sucMNZero {k = 4} 5 = zeroM {k = 4}             ; sz5 = Refl

-- modular +
infixl 6 +%
(+%) : {k : Nat} -> Mod k -> Mod k -> Mod k
(x ** _) +% mod = sucMN x mod

-- sample + calculations
plus_1_3 : Mod5_1 +% Mod5_3 = Mod5_4 ; plus_1_3 = Refl 
plus_1_0 : Mod5_1 +% zeroM {k = 4} = Mod5_1 ; plus_1_0 = Refl 
plus_0_2 : zeroM {k = 4} +% Mod5_2 = Mod5_2 ; plus_0_2 = Refl 
plus_3_2 : Mod5_3 +% Mod5_2 = zeroM {k = 4} ; plus_3_2 = Refl 
plus_many : Mod5_2 +% Mod5_2 +% Mod5_1 +% Mod5_4 +% Mod5_3 +% Mod5_1 = Mod5_3
plus_many = Refl 

-- modular inverse
invM : {k : Nat} -> Mod k -> Mod k
invM {k} (Z ** _) = zeroM {k}
invM {k} (S x ** _) = 
  let inv = minus k x
      bounded = lteToLtSuc inv k (minusLte k x)
  in (inv ** bounded)
 
-- sample inverse additions - proofs appear in ModGroupProperties
inv2 : (invM {k = 4} Mod5_2) = Mod5_3                     ; inv2 = Refl
plusInvZero : (invM (zeroM {k = 3})) +% zeroM = zeroM     ; plusInvZero = Refl
plusInv3 : (invM Mod5_3) +% Mod5_3 = zeroM {k = 4}        ; plusInv3 = Refl
plusInv1 : (invM Mod2_1) +% Mod2_1 = zeroM {k = 1}        ; plusInv1 = Refl

-- uniqueness : there is a unique modular number of a given modulus for a given natural number
uniqueM : {k : Nat} -> (mod1 : Mod k) -> (mod2 : Mod k) -> (naturalM mod1 = naturalM mod2) -> mod1 = mod2
uniqueM {k} (x ** bounded1) (x ** bounded2) Refl =
  cong {f = \bounded => (x ** bounded)} (uniqueLte bounded1 bounded2)

-- TODO. Simpler proof or obviate the need for proof.
-- proof that for values less than the maximum modular number sucM reduces to incM
sucMIsIncM : {k : Nat} -> (mod : Mod k) -> (ltk : LT (naturalM mod) k) -> sucM mod = incM mod ltk
sucMIsIncM {k} (x ** _)   ltk    with (decEq x k)
  sucMIsIncM {k} (x ** _) ltk       | (Yes xEqK) with ((ltToNe x k ltk) xEqK)
    sucMIsIncM {k} _      _         | (Yes xEqK)    | _ impossible
  sucMIsIncM {k} (x ** _) _         | (No xNeK) = uniqueM _ _ Refl

-- TODO. Simpler proof or obviate the need for proof.
-- proof that for the maximum modulr number sucM reduces to zeroM
sucMIsZeroM : {k : Nat} -> (mod : Mod k) -> (naturalM mod = k) -> sucM mod = ZeroM
sucMIsZeroM {k} (x ** xLteK) xEqK with (decEq x k) 
  sucMIsZeroM    _           _       | (Yes _) = Refl
  sucMIsZeroM    _           xEqK    | (No xNeK) with (xNeK xEqK)
    sucMIsZeroM  _           _       | (No xNeK)    | _ impossible

-- basic theorem 1
-- modular values can be obtained by repeated succession from modulr zero - as in naturals
sucMNIdentity : {k : Nat} -> (x : Nat) -> (xLtSk : LT x (S k)) -> sucMNZero {k} x = (x ** xLtSk)
sucMNIdentity {k = k} Z (LTESucc LTEZero) = Refl
sucMNIdentity {k = k} (S x) (LTESucc xLtK) =          -- xLtK : LTE (S x) k
  let xLtSk = ltToLtSuc x k xLtK                      -- xLtSk : LT x (S k)
  in 
    BEGIN
      (sucMNZero (S x)) ={}= 
      (sucM (sucMNZero x)) ={ (cong {f = sucM} (sucMNIdentity {k} x xLtSk)) }=
      (sucM {k} (x ** xLtSk)) ={}=
      (sucMInternal {k} (x ** xLtSk) (decEq x k)) ={ sucMIsIncM {k} (x ** xLtSk) xLtK }=
      (S x ** (LTESucc xLtK))
    QED

-- basic theorm 2
-- repeated application of sucM to zeroM cycles at the modulus [S k]
cycleZeroM : {k : Nat} -> sucMNZero {k} (S k) = ZeroM -- (0 ** LTESucc LTEZero)
cycleZeroM {k} =
  BEGIN
    (sucMNZero {k} (S k)) ={}=
    (sucM (sucMN k zeroM)) ={ cong {f = sucM} (sucMNIdentity {k} k lteRefl) }=
    (sucM (k ** lteRefl)) ={ sucMIsZeroM {k} (k ** lteRefl) Refl }=
    ZeroM -- (0 ** LTESucc LTEZero)
  QED

-- TODO. Rename to sucNMAdditive.
-- composition of iterated applications of modulo succession is additive in the number of iterations
sucMAdditive : {k : Nat} -> (x : Nat) -> (y : Nat) -> (mod : Mod k) -> ((sucMN x) . (sucMN y)) mod = sucMN (x + y) mod
sucMAdditive {k} x y mod = composeIterApplAdditive sucM x y mod

-- basic theorem 3
-- the sum of 2 modular numbers can be reduced to a function of the sum of their natural counterparts
modPlusByNatPlus : {k : Nat} -> (mod1 : Mod k) -> (mod2 : Mod k) -> 
  mod1 +% mod2 = sucMNZero (naturalM mod1 + naturalM mod2)
  
modPlusByNatPlus {k} (x ** _) (y ** yLteK) = 
  let applyAdditive = sucMAdditive x y zeroM
      applyIdentity = cong { f = sucMN x } (sym (sucMNIdentity y yLteK))
  in 
      BEGIN
        ((x ** _) +% (y ** yLteK)) ={}=
        (sucMN x (y ** yLteK)) ={ applyIdentity }=
        (sucMN x (sucMNZero y)) ={ applyAdditive }=
        (sucMN (x + y) zeroM) ={}=
        (sucMNZero (x + y))
      QED

-- commutativity of modulo addition
modPlusCommute : {k : Nat} -> (mx: Mod k) -> (my: Mod k) -> mx +% my = my +% mx
modPlusCommute {k} (x ** xLteK) (y ** yLteK) =
  BEGIN 
    ((sucM $^ x) (y ** yLteK)) ={ (modPlusByNatPlus {k} (x ** xLteK) (y ** yLteK)) }=
    (sucMNZero (x + y)) ={ ( cong {f = sucMNZero} (plusCommutative x y)) }=
    (sucMNZero (y + x)) ={ (sym ( modPlusByNatPlus {k} (y ** yLteK) (x ** xLteK))) }=
    ((sucM $^ y) (x ** xLteK))
  QED
  
