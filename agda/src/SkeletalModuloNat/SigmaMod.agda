
module SigmaMod where

open import Function using (_$_; _∋_; _∘_ )
open import Data.Nat using (ℕ; zero; suc; pred; _<_; _≤_; s≤s; z≤n; _+_; _∸_)
open import Agda.Builtin.Nat using (mod-helper)
open import Data.Nat.Properties using (<⇒≢; ≤∧≢⇒<; _≟_; +-assoc; +-comm; ≤-refl; m+n∸m≡n; n∸m≤n) 
open import Data.Product using (Σ; _,_)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; _≢_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _∎; _≡⟨⟩_; _≡⟨_⟩_)
open import Relation.Nullary using (¬_; Dec; yes; no)
open import Relation.Nullary.Negation using ()
open import UtilProperties using (≤⇒<suc; <-step; <-suc⇒≤;
  <-same-type⇒≡; <-same-sucs-type⇒≡; _$^_; $^-∘-additive)

{- Representation of modular numbers. Use "k + 1" as the modulus, since
there are no modular numbers for modulus zero.

For consistency, all functions and data structures are parameterized
by k, that is 1 less than the modulus. But for variable names we use
the modulus: mod₅1 : Mod 4 ; mod₅1 = (1, 1<5)  -}

Mod : ℕ → Set
Mod k = Σ ℕ (λ x → x < suc k)

-- the natural number corresponding to a modulo number
naturalₘ : {k : ℕ} → Mod k → ℕ
naturalₘ (x , _) = x

-- modular zero
postulate zeroₘ : {k : ℕ} → Mod k

-- Exercise. Define all numbers modulo 5.
-- We'll use these in later exercises.

{-
Succession for modular numbers: 
  naturalₘ mod < k => increment natutal value 
  naturalₘ mod ≡ k => zeroₘ
-}

-- helper function for successor
-- increment a modular number to the next higher modular number where possible
postulate incₘ : {k : ℕ} → (mod : Mod k) → (naturalₘ mod < k) → Mod k

-- helper function for successor
-- using a "with value" Dec (naturalₘ mod ≡ k)
postulate sucₘInternal : {k : ℕ} → (mod : Mod k) → Dec (naturalₘ mod ≡ k) → Mod k

-- modular successor
sucₘ : {k : ℕ} → Mod k → Mod k
sucₘ {k} mod@(x , x≤k) = sucₘInternal {k} mod (x ≟ k)

-- Exercise. Test succession modulo 5 for 1.
-- Exercise. Show that succession of 4 modulo 5 cycles back to zero.

-- repeated modular succession - _$^_ is repeated application
sucₘⁿ : {k : ℕ} →  (n : ℕ) → (mod : Mod k) → Mod k
sucₘⁿ {k} n = sucₘ $^ n

-- Exercise. Repeat succession 5 times on 0 modulo 5, and show that it is ≣ zeroₘ.
-- Exercise. Repeat for 2 modulo 5 and show that it is ≡ what you expect. 

-- shorthand for succession applied n times to modulo zero
sucₘⁿ-zeroₘ : {k : ℕ} → (n : ℕ) → Mod k
sucₘⁿ-zeroₘ {k} n = sucₘⁿ n zeroₘ

-- Exercise. Apply sucₘⁿ-zeroₘ to 0, 1, 2, 3, 4 to get all numbers modulo 5.
-- Show that the results are ≡ to the numbers modulo 5 you defined earlier.

-- modular +
infixl 6 _+ₘ_
_+ₘ_ : ∀ {k} → Mod k → Mod k → Mod k
(x , _) +ₘ mod = sucₘⁿ x mod

-- Exercise. Show by a few examples that +ₘ is well-defined for modulo arithmetic.

-- modular inverse
-ₘ_ : {k : ℕ} → Mod k → Mod k
-ₘ_ {k} (zero , x≤k) = (zero , x≤k)
-ₘ_ {k} (suc x , x≤k) = (k ∸ x , ≤⇒<suc (k ∸ x) k (n∸m≤n x k)) 

-- Exercise. Show by a couple of examples that adding a modular number to its inverse yiels zeroₘ.
-- To be proved later in the SigModGroupProperties module.

-- uniqueness : there is a unique modular number of a given modulus for a given natural number
postulate uniqueₘ : {k : ℕ} → (mod1 : Mod k) → (mod2 : Mod k) → naturalₘ mod1 ≡ naturalₘ mod2 → mod1 ≡ mod2

-- Prove that the successor of a modular number less than the maximum is obtained by incrementing it.
-- This is just a lemma that you may find useful in proving the properties of modular succession.
postulate sucₘ≡incₘ : {k : ℕ} → (mod : Mod k) → (x<k : naturalₘ mod < k) → sucₘ mod ≡ incₘ {k} mod x<k

-- Prove that the successor of the maximum modular number is zeroₘ.
-- Again you may find this lemma useful in your proofs.
postulate sucₘ≡zeroₘ : {k : ℕ} → (mod : Mod k) → (naturalₘ mod ≡ k) → sucₘ mod ≡ zeroₘ

-- basic theorem 1
-- modular values can be obtained by repeated succession from modular zero - as in naturals
postulate sucₘⁿ-identity : {k : ℕ} → (x : ℕ) → (x≤k : x < suc k) → sucₘⁿ-zeroₘ x ≡ (x , x≤k)

-- basic theorem 2
-- repeated application of sucₘ to zeroₘ cycles at the modulus [suc k]
postulate cycle-zeroₘ : {k : ℕ} → sucₘⁿ-zeroₘ {k} (suc k) ≡ zeroₘ

-- composition of iterated applications of modular succession is additive in the number of iterations
sucₘⁿ-additivity : {k : ℕ} → (x y : ℕ) → (mod : Mod k) → ((sucₘⁿ x) ∘ (sucₘⁿ y)) mod ≡ sucₘⁿ (x + y) mod
sucₘⁿ-additivity {k} x y mod = $^-∘-additive sucₘ x y mod

-- basic theorem 3
-- the sum of 2 modular numbers can be reduced to a function of the sum of their natural counterparts
-- hint - use sucₘⁿ-identity and sucₘⁿ-additivity
postulate +ₘ-by-+ : {k : ℕ} → (mod1 mod2 : Mod k) → mod1 +ₘ mod2 ≡ sucₘⁿ-zeroₘ {k} (naturalₘ mod1 + naturalₘ mod2)

-- commutativity of modular addition
-- hint : commutativity of modular numbers reduces to the commutativity of natural numbers
postulate +ₘ-comm : {k : ℕ} → (mod1 mod2 : Mod k) → mod1 +ₘ mod2 ≡ mod2 +ₘ mod1

