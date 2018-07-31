
import Data.Fin

implementation Sized (Fin m) where
  size = finToNat

total
succ: Fin m -> Fin m
succ {m = S k} x with (strengthen (FS x))
  succ {m = S k} x | (Left _) = FZ
  succ {m = S k} x | (Right sx) = sx

Z1 : Fin 1
Z1 = FZ

total
pred: Fin m -> Fin m
pred {m = S k} FZ = rewrite plusCommutative 1 k in (shift k Z1)
pred {m = S k} (FS x) = weaken x


{-
sizeRec : Sized a
  => (step : (x : a) -> ((y : a) -> Smaller y x -> b) -> b)
  -> (z : a)
  -> b
sizeRec step z = accRec step z (sizeAccessible z)
-}
-- (assert_smaller (modPlus (weaken x) y) x)

weakeEq : (x : Fin k) -> LTE (finToNat (weaken x)) (finToNat x)
weakeEq FZ = LTEZero
weakeEq (FS x) = LTESucc (weakeEq x)

step : (y : Fin m) -> (x1 : Fin m) -> ((y1 : Fin m) -> Smaller y1 x1 -> Fin m) -> Fin m
step y FZ _ = y
step _ (FS x) frec = succ (frec (weaken x) (LTESucc (weakeEq x)))



total
modPlus: Fin m -> Fin m -> Fin m
modPlus x y = sizeRec (step y) x
-- modPlus FZ y = y
-- modPlus (FS x) y = succ (modPlus (assert_smaller (weaken x) (FS x)) y)


infix 5 +++

total
(+++) : Fin m -> Fin m -> Fin m
(+++) x y = modPlus x y

total
modInverse : Fin m -> Fin m
modInverse {m = S k} FZ = FZ
modInverse {m = S k} (FS x) = pred $ modInverse (pred $ FS x)

propModLeftId : (x: Fin (S k)) -> modPlus FZ x = x
propModLeftId FZ = ?hole
propModLeftId (FS x) = ?propLeftId_rhs_2

propModRightId : (x: Fin (S k)) -> x +++ FZ = x
propModRightId FZ = ?propRightId_rhs_1
propModRightId (FS x) = ?propRightId_rhs_2

propModLeftInverse : (x: Fin (S k)) -> modInverse x +++ x = FZ

propModRightInverse : (x: Fin (S k)) -> x +++ modInverse x = FZ

propModAssociative : (x, y, z: Fin (S k)) -> x +++ (y +++ z) = (x +++ y) +++ z
