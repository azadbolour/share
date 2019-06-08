
module UtilProperties where

open import Function using (_$_; _∘_)
open import Data.Nat using (ℕ; zero; suc; pred; _<_; _≤_; s≤s; z≤n; _+_; _∸_)
open import Data.Nat.Properties using (+-assoc; +-comm; ≤-trans; <-trans; ≤-refl; ≤-step; n≤1+n; ≤∧≢⇒<; ≤⇒pred≤; <⇒≤; m+n∸m≡n; n∸m≤n) 
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; _≢_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _∎; _≡⟨⟩_; _≡⟨_⟩_)

-- shorthands for various inequality properties

<-suc : (a : ℕ) → a < suc a
<-suc _ = ≤-refl

≤⇒<suc : (a b : ℕ) → a ≤ b → a < suc b
≤⇒<suc a b a≤b = s≤s a≤b

≡⇒<suc : {a : ℕ} → (b : ℕ) → a ≡ b → a < suc b
≡⇒<suc {.x} x refl = <-suc x

<-step : (a b : ℕ) → a < b → a < suc b
<-step a b a<b = ≤-step a<b

<-suc⇒≤ : (a b : ℕ) → (a < suc b) → a ≤ b
<-suc⇒≤ a b (s≤s a≤b) = a≤b

<-zero-suc : (b : ℕ) → zero < suc b
<-zero-suc b = s≤s z≤n

<-suc-suc⇒< : (a b : ℕ) → suc a < suc b → a < b
<-suc-suc⇒< _ _ (s≤s a<b) = a<b

-- uniqueness of inequality proofs

-- two proofs of x < k are equal
<-same-type⇒≡ : {a b : ℕ} → (a<b₁ a<b₂ : a < b) → a<b₁ ≡ a<b₂
<-same-type⇒≡ {zero} {suc b} (s≤s (z≤n {b})) (s≤s (z≤n {b})) = refl
<-same-type⇒≡ {suc a} {suc b} (s≤s a<b₁) (s≤s a<b₂) = cong s≤s $ <-same-type⇒≡ {a} {b} a<b₁ a<b₂

-- two proofs of suc x < suc k are equal 
<-same-sucs-type⇒≡ : (a b : ℕ) → (sa<sb₁ sa<sb₂ : suc a < suc b) → sa<sb₁ ≡ sa<sb₂
<-same-sucs-type⇒≡ a b sa<sb₁ sa<sb₂ = <-same-type⇒≡ {suc a} {suc b} sa<sb₁ sa<sb₂

+-comm-+assoc : (x y z : ℕ) → (x + y) + z ≡ (y + z) + x
+-comm-+assoc x y z rewrite +-comm (y + z) x | +-assoc x y z = refl

-- iterated application of a function 
infixl 5 _$^_

_$^_ : {A : Set} → (f : A → A) → (n : ℕ) → (A → A)
(f $^ zero) a = a
(f $^ suc n) a = f $ (f $^ n) a

-- composition of iterated application is additive
$^-∘-additive : {A : Set} → (f : A → A) → (m n : ℕ) → (a : A) → ((f $^ m) ∘ (f $^ n)) a ≡ (f $^ (m + n)) a
$^-∘-additive f zero n a = refl
$^-∘-additive f (suc m) n a = cong f ($^-∘-additive f m n a)
