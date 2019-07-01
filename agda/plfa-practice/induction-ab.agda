
module induction-ab where

{-

Azad Bolour

This module compares rewrite proofs with equational reasoning proofs
for some of the properties of natural numbers treated in plfa Chapter
2.

Rewrite proofs are generally shorter than corresponding equational
proofs (and, as well, easier to develop in emacs). But rewrite proofs
are often harder to decipher on later reads.

To verify a rewrite proof, we need to mentally work out the reduced
goal that makes the match to the LHS of a rewrite possible. Commenting
a rewrite with the reduced goal that makes the rewrite possible, makes
it easier to verify by inspection.

On the other hand, the verbosity of an equational proofs can be
reduced by omitting proof steps that equate terms by simple function
application/reduction [≡⟨⟩ steps].

In many cases, the dual effects of commenting rewrites for
readability, and eliminating not-strictly-necessary proof steps in
equational reasoning, end up rendering the two approaches quite
comparable in readability and brevity.

Three styles of proofs are illustrated in this module:

1. Detailed equational proofs - use both of the equivalence operators
_≡⟨⟩_ and _≡⟨_⟩_. Each step of a detailed equational proof can be
trivially verified by inspection.

2. Minimal equational proofs - eliminate the use of the _≡⟨⟩_
operator, letting agda fill in the simple reduction-based
equivalences. A minmal equational proof starts with the LHS and ends
with the RHS of the reduced goal to be proved.

3. Commented rewrite proofs - for each rewrite, include a comment
stating the reduced goal being proved, for readability.

In cases where there is symmetry between the two sides of an equality
to be proved, equational proofs often become easier to construct and
understand by using "symmetric" reasoning.

In symmetric reasoning, the two sides of an equality are symmetrically
transformed until they become recognizably equal by a well-known
property. The LHS is transformed top-down and the RHS is transformed
bottom-up in the reasoning chain. And corresponding transformations
are the inverse of one another.
 
-}

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; cong₂; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)

-- Abbreviation Operators 

-- sym as an operator
infix 1 ←_
←_ : ∀ {ℓ} {A : Set ℓ} {a₁ a₂ : A} → a₁ ≡ a₂ → a₂ ≡ a₁
← refl = refl

-- cong as an operator
infix 6 _$≡_
_$≡_ : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} (f : A → B) {a b} → a ≡ b → f a ≡ f b
f $≡ refl = refl

-- cong of left + section (p +_) as an operator
infix 6 _+≡_
_+≡_ : {m n : ℕ} (p : ℕ) → m ≡ n → (p + m) ≡ (p + n)
p +≡ refl = refl

-- cong of right + section (_+ p) as an operator
infix 6 _≡+_
_≡+_ : {m n : ℕ} → m ≡ n → (p : ℕ) → (m + p) ≡ (n + p)
refl ≡+ p = refl

-- cong₂ of + as an operator
infix 6 _≡+≡_
_≡+≡_ : {m₁ n₁ m₂ n₂ : ℕ} → m₁ ≡ n₁ → m₂ ≡ n₂ → (m₁ + m₂) ≡ (n₁ + n₂)
refl ≡+≡ refl = refl

-- shorthands

suc² : ℕ → ℕ
suc² n = suc (suc n)

-- + right identity

-- detailed equational proof
+-identityᵣ : (n : ℕ) → n + zero ≡ n
+-identityᵣ zero = refl
+-identityᵣ (suc n) = begin
    suc n + zero       ≡⟨⟩
    suc (n + zero)     ≡⟨ suc $≡ +-identityᵣ n ⟩
    suc n              ∎

-- minimal equational proof
+-identityᵣ′ : (n : ℕ) → n + zero ≡ n
+-identityᵣ′ zero = refl
+-identityᵣ′ (suc n) = begin
    suc (n + zero)     ≡⟨ suc $≡ +-identityᵣ′ n ⟩
    suc n              ∎

-- + rewrite proof
+-identityᵣ″ : (n : ℕ) → n + zero ≡ n
+-identityᵣ″ zero = refl
+-identityᵣ″ (suc n) -- suc (n + zero) ≡ suc n
  rewrite +-identityᵣ″ n = refl

-- right addition of suc

-- detailed equational proof
+-sucʳ : (a b : ℕ) → a + suc b ≡ suc (a + b)
+-sucʳ zero b = refl
+-sucʳ (suc a) b = begin
  suc a + suc b ≡⟨⟩
  suc (a + suc b)       ≡⟨ suc $≡ +-sucʳ a b ⟩
  suc (suc (a + b))     ≡⟨⟩
  suc (suc a + b)       ∎

-- minimal equational proof
+-sucʳ′ : (a b : ℕ) → a + suc b ≡ suc (a + b)
+-sucʳ′ zero b = refl
+-sucʳ′ (suc a) b = begin
  suc (a + suc b)       ≡⟨ suc $≡ +-sucʳ′ a b ⟩
  suc (suc a + b)       ∎

-- rewrite proof
+-sucʳ‴ : (a b : ℕ) → a + suc b ≡ suc (a + b)
+-sucʳ‴ zero b = refl
+-sucʳ‴ (suc a) b -- suc (a + suc b) ≡ suc (suc (a + b))
  rewrite +-sucʳ‴ a b = refl 

-- simplify suc a + suc b

-- detailed equational proof
+-suc-suc : (a b : ℕ) → suc a + suc b ≡ suc² (a + b)
+-suc-suc zero b = refl
+-suc-suc (suc a) b = begin
    suc (suc a) + suc b       ≡⟨⟩
    suc (suc a + suc b)       ≡⟨ suc $≡ +-suc-suc a b ⟩
    suc (suc² (a + b))        ≡⟨⟩
    suc² (suc a + b)          ∎

-- minimal equational proof
+-suc-suc′ : (a b : ℕ) → suc a + suc b ≡ suc² (a + b)
+-suc-suc′ zero b = refl
+-suc-suc′ (suc a) b = begin
    suc (suc a + suc b)       ≡⟨ suc $≡ +-suc-suc′ a b ⟩
    suc² (suc a + b)          ∎

-- alternate minimal equatinal proof
+-suc-suc″ : (a b : ℕ) → suc a + suc b ≡ suc² (a + b)
+-suc-suc″ zero b = refl
+-suc-suc″ (suc a) b = begin
    suc (suc (a + suc b))       ≡⟨ suc² $≡ +-sucʳ a b ⟩
    suc² (suc (a + b))          ∎

-- rewrite proof
-- note - it does not use the fully-normalized goal
+-suc-suc‴ : (a b : ℕ) → suc a + suc b ≡ suc² (a + b)
+-suc-suc‴ zero b = refl
+-suc-suc‴ (suc a) b -- suc (suc a + suc b) ≡ suc (suc (suc (a + b)))
  rewrite +-suc-suc‴ a b = refl

-- + commutativity
+-comm : (a b : ℕ) → a + b ≡ b + a
-- the zero cases easily reduce to right identity
+-comm zero b -- goal: b ≡ b + 0
  = ← +-identityᵣ b
+-comm (suc a) zero -- suc (a + zero) ≡ suc a
  = suc $≡ +-identityᵣ a
-- symmetric equational proof
+-comm (suc a) (suc b) = begin
    suc a + suc b    ≡⟨ +-suc-suc a b ⟩
    suc² (a + b)     ≡⟨ suc² $≡ +-comm a b ⟩
    suc² (b + a)     ≡⟨ ← +-suc-suc b a ⟩
    suc b + suc a    ∎

-- rewrite proof can easily be derived from the equational proof

-- + associativity

-- detailed equational proof
+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p {- n + p ≡ n + p -} = refl
-- symmetric equational proof
+-assoc (suc m) n p = begin
    (suc m + n) + p       ≡⟨⟩
    suc (m + n) + p       ≡⟨⟩
    suc ((m + n) + p)     ≡⟨ suc $≡ +-assoc m n p ⟩
    suc (m + (n + p))     ≡⟨⟩
    suc m + (n + p)       ∎

-- minimal equational proof
+-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc′ zero n p {- n + p ≡ n + p -} = refl
+-assoc′ (suc m) n p = begin
    suc ((m + n) + p)     ≡⟨ suc $≡ +-assoc′ m n p ⟩
    suc m + (n + p)       ∎

-- rewrite proof
+-assoc″ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc″ zero n p {- n + p ≡ n + p -} = refl
+-assoc″ (suc m) n p -- suc ((m + n) + p) ≡ suc (m + (n + p))
  rewrite +-assoc″ m n p = refl

-- cong proof of + associativity
+-assoc‴ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc‴ zero n p {- n + p ≡ n + p -} = refl
+-assoc‴ (suc m) n p -- suc ((m + n) + p) ≡ suc (m + (n + p))
  = suc $≡ +-assoc‴ m n p

-- swap parameters in a nested +

-- symmetric equational proof
+-swap : (m n p : ℕ) → m + (n + p) ≡ n + (m + p)
+-swap m n p = begin
    m + (n + p)     ≡⟨ ← +-assoc m n p ⟩
    (m + n) + p     ≡⟨ +-comm m n ≡+ p ⟩
    (n + m) + p     ≡⟨ +-assoc n m p ⟩
    n + (m + p)     ∎

-- rewrite proof
-- again the proof is basically isomorphic to the equational proof
-- and as usual, it is not as easily verifiable by inspection
-- adding comments for each rewrite would make it more easily verifiable
-- but the comments overly clutter the code
+-swap′ : (m n p : ℕ) → m + (n + p) ≡ n + (m + p)
+-swap′ m n p rewrite (sym (+-assoc m n p)) |
                      +-comm m n            |
                      +-assoc n m p = refl

{-
By now it is clear that minimal equational proofs and rewrite proofs
are basically isomorphic and can easily be translated to one
another. Also, that the minimal and detailed equational proofs can
easily be translated to one another. Hence we'll omit some versions
of the proofs below.
-}

-- swap parameters in a nested + - "right" version
+-swapᵣ : (m n p : ℕ) → m + (p + n) ≡ n + (p + m)
+-swapᵣ m n p rewrite +-comm p n | +-comm p m = +-swap m n p 

*-zeroᵣ : (n : ℕ) → n * zero ≡ zero
*-zeroᵣ zero {- zero ≡ zero -} = refl
*-zeroᵣ (suc n) {- n * zero ≡ zero -} = *-zeroᵣ n

{- 
Declaration of commutativity of *. Used in lemmas for proving
commutativity. The implementations of the lemmas and * commutativity
are mutually recursive.  
-}
*-comm : (m n : ℕ) → m * n ≡ n * m

-- lemma - multiply by suc to the right

-- detailed equational proof
*-sucᵣ : (m n : ℕ) → m * suc n ≡ m + m * n
*-sucᵣ m n = begin
  m * suc n      ≡⟨ *-comm m (suc n) ⟩
  suc n * m      ≡⟨⟩
  m + n * m      ≡⟨ m +≡ *-comm n m ⟩
  m + m * n      ∎

-- minimal equational proof
*-sucᵣ′ : (m n : ℕ) → m * suc n ≡ m + m * n
*-sucᵣ′ m n = begin
  m * suc n      ≡⟨ *-comm m (suc n) ⟩
  m + n * m      ≡⟨ m +≡ *-comm n m ⟩
  m + m * n      ∎

-- lemma - multiply out (suc m) * (suc n)

-- detailed equational proof
*-suc-suc :  (m n : ℕ) → suc m * suc n ≡ suc (m + n) + m * n
*-suc-suc m n = begin
    suc m * suc n           ≡⟨⟩
    suc n + m * suc n       ≡⟨ suc n +≡ *-sucᵣ m n ⟩
    suc n + (m + m * n)     ≡⟨ ← +-assoc (suc n) m (m * n) ⟩
    (suc n + m) + m * n     ≡⟨⟩
    suc (n + m) + m * n     ≡⟨ (suc $≡ +-comm n m) ≡+ m * n ⟩
    suc (m + n) + m * n     ∎
    
-- minimal equational proof
*-suc-suc′ :  (m n : ℕ) → suc m * suc n ≡ suc (m + n) + m * n
*-suc-suc′ m n = begin
    suc n + m * suc n       ≡⟨ suc n +≡ *-sucᵣ m n ⟩
    suc n + (m + m * n)     ≡⟨ ← +-assoc (suc n) m (m * n) ⟩
    suc (n + m) + m * n     ≡⟨ (suc $≡ +-comm n m) ≡+ m * n ⟩
    suc (m + n) + m * n     ∎

-- * commutativity
*-comm zero m = sym (*-zeroᵣ m)
*-comm (suc m) zero = *-zeroᵣ (suc m)
-- symmetric equational proof
*-comm (suc m) (suc n) = begin
    suc m * suc n               ≡⟨ *-suc-suc m n ⟩
    suc (m + n) + (m * n)       ≡⟨ (suc $≡ (+-comm m n)) ≡+≡ *-comm m n ⟩
    suc (n + m) + (n * m)       ≡⟨ ← *-suc-suc n m ⟩
    suc n * suc m               ∎

-- braided +: sum of 2x2 array is independent of summing by rows or by columns first
-- rewrite proof
+-braid : (m₁ m₂ n₁ n₂ : ℕ) → (m₁ + n₁) + (m₂ + n₂) ≡ (m₁ + m₂) + (n₁ + n₂)
+-braid zero m₂ n₁ n₂ -- n₁ + (m₂ + n₂) ≡ m₂ + (n₁ + n₂)
  = +-swap n₁ m₂ n₂
+-braid (suc m₁) m₂ n₁ n₂ -- suc (m₁ + n₁ + (m₂ + n₂)) ≡ suc (m₁ + m₂ + (n₁ + n₂))
  rewrite +-braid m₁ m₂ n₁ n₂ = refl

-- right distribution of * over +

-- detailed equational proof
*-distrib-+ᵣ : (m n p : ℕ) → p * (m + n) ≡ (p * m) + (p * n)
*-distrib-+ᵣ m n zero = refl
*-distrib-+ᵣ m n (suc p) = begin
    (suc p) * (m + n)               ≡⟨⟩
    (m + n) +  p * (m + n)          ≡⟨ (m + n) +≡ *-distrib-+ᵣ m n p ⟩
    (m + n) + (p * m + p * n)       ≡⟨ +-braid m (p * m) n (p * n) ⟩
    (m + p * m) + (n + p * n)       ≡⟨⟩
    (suc p * m) + (suc p * n)       ∎

-- minimal equational proof
*-distrib-+ᵣ′ : (m n p : ℕ) → p * (m + n) ≡ (p * m) + (p * n)
*-distrib-+ᵣ′ m n zero = refl
*-distrib-+ᵣ′ m n (suc p) = begin
    (m + n) +  p * (m + n)          ≡⟨ (m + n) +≡ *-distrib-+ᵣ′ m n p ⟩
    (m + n) + (p * m + p * n)       ≡⟨ +-braid m (p * m) n (p * n) ⟩
    (suc p * m) + (suc p * n)       ∎

-- left distribution of * over +
*-distrib-+ₗ : (m n p : ℕ) → (m + n) * p ≡ (m * p) + (n * p)
*-distrib-+ₗ m n p rewrite (*-comm (m + n) p) | (*-comm m p) | (*-comm n p) = *-distrib-+ᵣ m n p

-- * associativity

-- detailed equational proof
*-assoc : (m n p : ℕ) → (m * n) * p ≡ m * (n * p)
*-assoc zero n p = refl
*-assoc (suc m) n p = begin
   ((suc m) * n) * p       ≡⟨⟩
   (n + m * n) * p         ≡⟨ *-distrib-+ₗ n (m * n) p  ⟩
   n * p + m * n * p       ≡⟨ n * p +≡ *-assoc m n p  ⟩
   n * p + m * (n * p)     ≡⟨⟩
   (suc m) * (n * p)       ∎

-- minimal equational proof
*-assoc′ : (m n p : ℕ) → (m * n) * p ≡ m * (n * p)
*-assoc′ zero n p = refl
*-assoc′ (suc m) n p = begin
   (n + m * n) * p         ≡⟨ *-distrib-+ₗ n (m * n) p  ⟩
   n * p + m * n * p       ≡⟨ n * p +≡ *-assoc′ m n p  ⟩
   (suc m) * (n * p)       ∎

