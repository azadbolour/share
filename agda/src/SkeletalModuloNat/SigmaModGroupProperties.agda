
module SigmaModGroupProperties where

open import Data.Nat using (ℕ; zero; suc; pred; _<_; _≤_; s≤s; z≤n; _+_; _∸_)
open import Data.Nat.Properties using (+-assoc; +-comm; n≤1+n; <⇒≤; m+n∸m≡n; n∸m≤n; m∸n+n≡m) 
open import Data.Product using (_,_)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; _≢_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _∎; _≡⟨⟩_; _≡⟨_⟩_)
open import UtilProperties using (≤⇒<suc; <-suc-suc⇒<; +-comm-+assoc)
open import SigmaMod using (Mod; zeroₘ; sucₘ; sucₘⁿ; _+ₘ_; -ₘ_; +ₘ-by-+; naturalₘ; 
  sucₘⁿ-additivity ; +ₘ-comm; sucₘⁿ-zeroₘ; cycle-zeroₘ)

{-
Proof of group axioms for modular +.

One general strategy for proofs: +ₘ can be reduced to a function
of + for naturals by using +ₘ-by-+. So properties of +ₘ can be reduced
to properties of + for naturals.
-}

-- group left identity law for +ₘ
postulate +ₘ-identityˡ : {k : ℕ} → (mod : Mod k) → zeroₘ +ₘ mod ≡ mod

-- group right identity law for +ₘ
postulate +ₘ-identityʳ : {k : ℕ} → (mod : Mod k) → mod +ₘ zeroₘ ≡ mod

-- variant of +/∸ cancellation theorem
sm<sn⇒m+n∸m≡n : {k : ℕ} → (x : ℕ) → (sx<sk : suc x < suc k) → x + (k ∸ x) ≡ k
sm<sn⇒m+n∸m≡n {k} x sx≤sk = let x<k = <-suc-suc⇒< x k sx≤sk in m+n∸m≡n {x} {k} (<⇒≤ x<k)

-- variant of +/∸ cancellation theorem
sm<sn⇒n∸m+sm≡sn : {k : ℕ} → (x : ℕ) → (sx<sk : suc x < suc k) → (k ∸ x) + suc x ≡ suc k
sm<sn⇒n∸m+sm≡sn  {k} x sx<sk rewrite +-comm (k ∸ x) (suc x) | sm<sn⇒m+n∸m≡n {k} x sx<sk = refl 

-- group left inverse law for +ₘ
-- hint - use sm<sn⇒n∸m+sm≡sn
--        and cycle-zeroₘ [: {k : ℕ} → sucₘⁿ-zeroₘ (suc k) ≡ zeroₘ]
postulate +ₘ-inverseˡ : {k : ℕ} → (mod : Mod k) → ( -ₘ mod ) +ₘ mod ≡ zeroₘ

-- group right inverse law for +ₘ
postulate +ₘ-inverseʳ : {k : ℕ} → (mod : Mod k) → mod +ₘ ( -ₘ mod ) ≡ zeroₘ

-- helper lemma for associativity
-- nested modulo plus is a function of nested natural plus for the corresponding natural numbers
postulate +ₘ-+ₘ-by-+-+ : {k : ℕ} → (mx my mz : Mod k) → (mx +ₘ my) +ₘ mz ≡ sucₘⁿ-zeroₘ ((naturalₘ mx + naturalₘ my) + naturalₘ mz)

-- group associativity law for +ₘ
postulate +ₘ-assoc : {k : ℕ} → (mx my mz : Mod k) → (mx +ₘ my) +ₘ mz ≡ mx +ₘ (my +ₘ mz)

