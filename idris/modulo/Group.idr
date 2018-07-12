
module Group

-- Algebraic group as an interface including the group axioms as propositions.
public export
interface Group g where
  -- Structure.
  id : g
  inv : g -> g
  (+) : g -> g -> g
  -- Axioms.
  propAssoc : (a, b, c: g) -> a + (b + c) = (a + b) + c
  propLeftId : (x: g) -> id + x = x
  propRightId : (x: g) -> x + id = x
  propLeftInv : (x: g) -> inv x + x = id
  propRightInv : (x: g) -> x + inv x = id
