|||
||| Modular arithmetic numbers as records.
|||
||| Ideally modulo dependent pairs (see ModularNat.idr) should be
||| sufficient for representing modular numbers. But I found them
||| sometimes hard to work with. Hence the use of a separate data type.
||| TODO. Unclear whether the additional complexity of adding
||| this representation is really necessary or all that useful.
|||
module ModData

import Group
import ModularNat

||| Modular arithmetic numbers as records.
public export
data Mod = MkMod (num: Nat ** LtModulus num)

||| Addition for modular arithmetic numbers as records.
public export
(+) : Mod -> Mod -> Mod
(+) (MkMod mx) (MkMod my) = MkMod (mx + my)

||| Modular zero as a record.
public export
modZ : Mod
modZ = MkMod zModulo

||| Modular zero as the addition identity.
public export
ModId: Mod
ModId = modZ

||| The addition inverse of of a modular integer as a record.
public export
total
modInverse : Mod -> Mod
modInverse (MkMod mx) = MkMod (inverseModulo mx)

{-
Propositions for arithmetic on modular integers represented as records.
-}

||| Zero is the left identity of modular addition.
propModPlusLeftId : (modX: Mod) -> (ModId + modX) = modX
propModPlusLeftId (MkMod (x ** xLtModulus)) = Refl

||| Zero is the right identity of modular addition.
||| The implementation is delegated to the corresponding proposition
||| on modular dependent pairs.
propModPlusRightId : (modX: Mod) -> (modX + ModId) = modX
propModPlusRightId (MkMod (x ** xLtModulus)) =
  let reducedDifferentialGoal = (plusModulo' x xLtModulus 0 ModularNat.modulusPositive = (x ** xLtModulus))
      reducedDifferentialPrf: reducedDifferentialGoal = ModularNat.propPlusModuloRightId (x ** xLtModulus)
  in rewrite reducedDifferentialPrf in Refl

||| Modular addition is commutative.
propModPlusCommutative : (modX, modX: Mod) ->
  modX + modY = modY + modX
propModPlusCommutative (MkMod (x ** xLtModulus)) modY = ?propModPlusCommutative_rhs_2

||| Modular addition is associative.
propModPlusAssociative : (modX, modY, modZ: Mod) ->
  modX + (modY + modZ) = (modX + modY) + modZ

||| Adding the inverse to the left yields the identity value.
propModPlusLeftInverse : (modX : Mod) -> (modInverse modX + modX) = ModId

||| Adding the inverse to the right yields the identity value.
propModPlusRightInverse : (modX : Mod) -> (modX + modInverse modX) = ModId

public export
Group Mod where
  id = ModId
  inv = modInverse
  (+) = (+)
  propAssoc = propModPlusAssociative
  propLeftId = propModPlusLeftId
  propRightId = propModPlusRightId
  propLeftInv = propModPlusLeftInverse
  propRightInv = propModPlusRightInverse
