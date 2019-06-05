
module UtilProperties where

open import Function using (_$_; _∘_)
open import Data.Nat using (ℕ; zero; suc; pred; _<_; _≤_; s≤s; z≤n; _+_; _∸_)
open import Data.Nat.Properties using (+-assoc; +-comm; ≤-trans; <-trans; ≤-refl; ≤-step; n≤1+n; ≤∧≢⇒<; ≤⇒pred≤; <⇒≤; m+n∸m≡n; n∸m≤n) 
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; _≢_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _∎; _≡⟨⟩_; _≡⟨_⟩_)

{-
Shorthands for various properties of < and its relation to ≡ and ≤,
which are not provided in the given exact form in stdlib.

You may find them useful in proofs dealing with the manipulation of
modular numbers as the < relation plays a central role in their
represention.

< is defined as: m < n = suc m ≤ n

For properties provided by stdlib, see: 
  https://agda.github.io/agda-stdlib/Data.Nat.Properties.html
-}

postulate <-suc : (a : ℕ) → a < suc a
postulate ≤⇒<suc : (a b : ℕ) → a ≤ b → a < suc b
postulate ≡⇒<suc : {a : ℕ} → (b : ℕ) → a ≡ b → a < suc b
postulate <-step : (a b : ℕ) → a < b → a < suc b
postulate <-suc⇒≤ : (a b : ℕ) → (a < suc b) → a ≤ b
postulate <-zero-suc : (b : ℕ) → zero < suc b

-- hint - case split on <
postulate <-suc-suc⇒< : (a b : ℕ) → suc a < suc b → a < b

-- uniqueness of inequality proofs

-- two proofs of x < k are equal
postulate <-same-type⇒≡ : {a b : ℕ} → (a<b₁ a<b₂ : a < b) → a<b₁ ≡ a<b₂

-- two proofs of suc x < suc k are equal 
postulate <-same-sucs-type⇒≡ : (a b : ℕ) → (sa<sb₁ sa<sb₂ : suc a < suc b) → sa<sb₁ ≡ sa<sb₂

-- combination of commutativity and associativity
postulate +-comm-+assoc : (x y z : ℕ) → (x + y) + z ≡ (y + z) + x

-- iterated application of a function

infixl 5 _$^_

_$^_ : {A : Set} → (f : A → A) → (n : ℕ) → (A → A)
(f $^ zero) a = a
(f $^ suc n) a = f $ (f $^ n) a


-- composition of iterated applications is additive
postulate $^-∘-additive : {A : Set} → (f : A → A) → (m n : ℕ) → (a : A) → ((f $^ m) ∘ (f $^ n)) a ≡ (f $^ (m + n)) a
