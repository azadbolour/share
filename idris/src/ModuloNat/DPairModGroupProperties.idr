
-- group properties of modular numbers represented as dependent pairs
module DPairModGroupProperties

import PropositionalEquality
import UtilProperties
import DPairMod

-- group left identity for +%
modPlusLeftIdentity : {k : Nat} -> (mod : Mod k) -> ZeroM +% mod = mod
modPlusLeftIdentity mod = BEGIN (ZeroM +% mod) ={}= (sucMN 0 mod) ={}= mod QED

-- group right identity for +%
modPlusRightIdentity : {k : Nat} -> (mod : Mod k) -> mod +% ZeroM = mod
modPlusRightIdentity mod = 
  BEGIN
    (mod +% ZeroM) ={ (modPlusCommute mod ZeroM) }=
    (ZeroM +% mod) ={ (modPlusLeftIdentity mod) }=
    mod
  QED

cancelPlusMinusSuc : (m : Nat) -> (n : Nat) -> (LTE m n) -> (minus n m) + (S m) = S n
cancelPlusMinusSuc m n mLteN = 
  BEGIN
    ((minus n m) + (S m)) ={ (plusCommutative (minus n m) (S m)) }=
    (S (m + minus n m)) ={ (cong {f = S} (cancelPlusMinus m n mLteN)) }=
    (S n)
  QED

-- TODO. Unclutter this proof.
-- group left inverse law for +%
modPlusLeftInverse : {k : Nat} -> (mod : Mod k) -> invM mod +% mod = ZeroM
modPlusLeftInverse {k} (Z ** LTESucc LTEZero) = Refl
modPlusLeftInverse {k} (S x ** sXLteK) =
  let xLteK = ltToLte x k (fromLtSucs x k sXLteK)
      canceller : ((minus k x) + S x = S k) = cancelPlusMinusSuc x k xLteK 
      applyCancel = cong {f = sucMNZero} canceller
  in
  BEGIN
    ((invM (S x ** sXLteK)) +% (S x ** sXLteK)) ={}=
    ((sucM $^ minus k x) (S x ** sXLteK)) ={ (modPlusByNatPlus (minus k x ** minusLtSuc x k) (S x ** sXLteK)) }=
    (sucMNZero ((minus k x) + S x)) ={ applyCancel }=
    (sucMNZero (S k)) ={ cycleZeroM }=
    (0 ** LTESucc LTEZero)
  QED
  
-- group right inverse law for +%
modPlusRightInverse : {k : Nat} -> (mod : Mod k) -> mod +% invM mod = ZeroM
modPlusRightInverse {k} mod = 
  BEGIN 
    (mod +% invM mod) ={ (modPlusCommute mod (invM mod)) }=
    (invM mod +% mod) ={ (modPlusLeftInverse mod) }=
    (0 ** LTESucc LTEZero)
  QED

-- -- modulo plus applied twice is a function of natural number plus applied twice
modPlusTwiceAsNatPlusTwice : {k : Nat} -> (mx : Mod k) -> (my : Mod k) -> (mz : Mod k) -> 
  (mx +% my) +% mz = sucMNZero ((naturalM mx + naturalM my) + naturalM mz)
  
modPlusTwiceAsNatPlusTwice {k} (x ** bx) (y ** by) (z ** bz) =
  let mx = (x ** bx)
      my = (y ** by)
      mz = (z ** bz)
  in
    BEGIN
      (mx +% my +% mz) ={ (cong {f = (+% mz) } (modPlusByNatPlus mx my)) }=
      (sucMNZero (x + y) +% mz) ={ (modPlusCommute (sucMNZero (x + y)) mz) }=
      (mz +% sucMNZero (x + y)) ={}=
      (sucMN z (sucMN (x + y) zeroM)) ={ (sucMAdditive z (x + y) zeroM) }=
      (sucMNZero (z + (x + y))) ={ (cong {f = sucMNZero} (plusCommutative z (x + y))) }= 
      ((sucM $^ (x + y) + z) (0 ** LTESucc LTEZero))
    QED

-- associativity for +%
modPlusAssoc : {k : Nat} -> (mx : Mod k) -> (my : Mod k) -> (mz : Mod k) -> 
  (mx +% my) +% mz = mx +% (my +% mz)

modPlusAssoc {k} (x ** bx) (y ** by) (z ** bz) =
  let mx = (x ** bx)
      my = (y ** by)
      mz = (z ** bz)
  in 
    BEGIN
      ((mx +% my) +% mz) ={ (modPlusTwiceAsNatPlusTwice mx my mz) }= 
      (sucMNZero ((x + y) + z)) ={ (cong {f = sucMNZero} (plusCommWithPlusAssoc x y z)) }=
      (sucMNZero ((y + z) + x)) ={ (sym (modPlusTwiceAsNatPlusTwice my mz mx )) }=
      ((my +% mz) +% mx) ={ (modPlusCommute (my +% mz) mx) }=
      (mx +% (my +% mz))
    QED
