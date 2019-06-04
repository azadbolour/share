
-- basic properties used in proving properties of modular naturals

module UtilProperties

%default total
%access public export

-- shorthands to reduce bulk
zn : {right : Nat} -> LTE Z right
zn = LTEZero
ss : {left : Nat} -> {right : Nat} -> (LTE left right) -> LTE (S left) (S right) 
ss = LTESucc 

-- various inequality lemmas

-- a number is less than its successor
ltSuc : (a : Nat) -> LT a (S a)
ltSuc a = lteRefl

-- (= n) => (< S n)
eqToLtSuc : (a : Nat) -> (b : Nat) -> (a = b) -> (LT a (S b))
eqToLtSuc a b aEqB = LTESucc (rewrite aEqB in lteRefl) -- TODO. Simpler proof?

-- (<= n) => (< S n) 
lteToLtSuc : (a : Nat) -> (b : Nat) -> (LTE a b) -> (LT a (S b))
lteToLtSuc a b aLteB = LTESucc aLteB

-- (< n) => (<= n)
ltToLte : (a : Nat) -> (b : Nat) -> (LT a b) -> (LTE a b)
ltToLte a b aLtB = lteSuccLeft aLtB

-- (< n) => (< S n)
ltToLtSuc : (a : Nat) -> (b : Nat) -> (LT a b) -> (LT a (S b))
ltToLtSuc a b aLtB = lteSuccRight aLtB

-- (< S n) => (<= n)
ltSucToLte : (a : Nat) -> (b : Nat) -> (LT a (S b)) -> (LTE a b)
ltSucToLte a b aLtSB = fromLteSucc aLtSB

-- 0 < S n
zeroLtSuc : (b : Nat) -> (LT Z (S b)) 
zeroLtSuc b = LTESucc LTEZero

-- (S m < S n) => (m < n)
fromLtSucs : (a : Nat) -> (b : Nat) -> (LT (S a) (S b)) -> (LT a b)
fromLtSucs a b (LTESucc aLtB) = aLtB

-- (S m /= S n) => (m /= n) 
nseToNe : (a : Nat) -> (b : Nat) -> Not (S a = S b) -> Not (a = b)
nseToNe a b nse = \eq0Sb => nse $ cong {f = S} eq0Sb  -- TODO. How to use absurd instead?

-- (< n) => (/= n)
ltToNe : (m : Nat) -> (n : Nat) -> (LT m n) -> Not (m = n)
ltToNe Z (S n) (LTESucc LTEZero) = absurd
ltToNe (S m) (S n) (LTESucc mLtN) = \smEqSn =>
  let mEqN = succInjective m n smEqSn
  in  (ltToNe m n mLtN) mEqN
  
-- (<= n, /= n) => (< n) 
lteNeToLt : (a : Nat) -> (b : Nat) -> (LTE a b) -> (Not (a = b)) -> (LT a b) 
lteNeToLt Z Z LTEZero aNeB = absurd (aNeB Refl)
lteNeToLt Z (S k) LTEZero aNeB = LTESucc LTEZero
lteNeToLt (S a) (S b) (LTESucc aLteB) saNeSb = 
  let aNeB = nseToNe a b saNeSb 
      aLtB = lteNeToLt a b aLteB aNeB
  in LTESucc aLtB

-- (m <= n) has a unique inhabitant
uniqueLte : {m : Nat} -> {n : Nat} -> (lte1 : LTE m n) -> (lte2 : LTE m n) -> lte1 = lte2
uniqueLte LTEZero LTEZero = Refl
uniqueLte (LTESucc lte1) (LTESucc lte2) = cong {f = LTESucc} (uniqueLte lte1 lte2)

-- subtraction of a value <= n is cancelled by its addition
cancelPlusMinus : (m : Nat) -> (n : Nat) -> (LTE m n) -> m + (minus n m) = n
cancelPlusMinus Z n LTEZero = minusZeroRight n
cancelPlusMinus (S m) (S n) (LTESucc mLteN) = cong {f = S} (cancelPlusMinus m n mLteN)

-- subtraction does not increase a value (<= version)
minusLte : (n : Nat) -> (m : Nat) -> LTE (minus n m) n
minusLte Z n = LTEZero
minusLte (S m) Z = lteRefl
minusLte (S m) (S n) = lteSuccRight $ minusLte m n

-- subtraction does not increase a value (< version)
minusLtSuc : (m : Nat) -> (n : Nat) -> LT (minus n m) (S n)
minusLtSuc m n = lteToLtSuc (minus n m) n (minusLte n m)

-- combination of commutativity and associativity for +
plusCommWithPlusAssoc : (x : Nat) -> (y : Nat) -> (z : Nat) -> (x + y) + z = (y + z) + x
plusCommWithPlusAssoc x y z = rewrite plusCommutative (y + z) x in (rewrite plusAssociative x y z in Refl)

-- iterated application of a function
infixl 5 $^ 
($^) : {a : Type} -> (f : a -> a) -> (n : Nat) -> (a -> a)
f $^ Z = \x => x
f $^ (S n) = f . (f $^ n)

-- composition of iterated application is additive in the number of iterations
composeIterApplAdditive : {a : Type} -> 
  (f : a -> a) -> (m, n : Nat) -> (x : a) -> ((f $^ m) . (f $^ n)) x = (f $^ (m + n)) x
  
composeIterApplAdditive f Z n x = Refl
composeIterApplAdditive f (S m) n x = 
  rewrite composeIterApplAdditive f m n x in Refl 
  -- TODO. What is the type of the Refl here?

