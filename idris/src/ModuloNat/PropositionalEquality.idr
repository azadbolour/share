
{-
implementation of equaltional reasoning a la Agda in Idris - fashioned after

  https://gist.github.com/rodrigogribeiro/6f74c50ef767b8c07c9b771981b6e08a
  https://github.com/agda/agda-stdlib/blob/master/src/Relation/Binary/PropositionalEquality/Core.agda
-}
module PropositionalEquality

%default total
%access public export



syntax BEGIN [eq] = eqId eq
syntax [expr] "QED" = qed expr 
syntax [x] "={}=" [xy] = eqId xy
syntax [x] "={" [xy] "}=" [yz] = step x xy yz

namespace EqReasoning
  using (a : Type, x : a, y : a, z : a)
  qed : (x : a) -> x = x
  qed x = Refl
  
  step : (x : a) -> x = y -> y = z -> x = z
  step x Refl Refl = Refl
  
  eqId : (x = y) -> (x = y)
  eqId Refl = Refl

test1 : (x : Nat) -> (x = x)
test1 x = 
  BEGIN
    x
  QED
  
test2 : (3 = 3)
test2 = 
  BEGIN
    3 ={ Refl }=
    3
  QED
  
test3 : (3 = 3)
test3 = 
  BEGIN
    3 ={}=
    3
  QED
  
