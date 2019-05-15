
module UtilProperties where

open import Function using (_$_)
open import Data.Nat using (ℕ; zero; suc; pred; _<_; _≤_; s≤s; z≤n; _+_; _∸_)
open import Data.Nat.Properties using (+-assoc; +-comm; ≤-trans; <-trans; ≤-refl; ≤-step; n≤1+n; ≤∧≢⇒<; ≤⇒pred≤; <⇒≤; m+n∸m≡n; n∸m≤n) 
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; _≢_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _∎; _≡⟨⟩_; _≡⟨_⟩_)

-- various ineqality lemmas

<-suc : (a : ℕ) → a < suc a
<-suc k = s≤s ≤-refl

≤-cong-suc : (a b : ℕ) → a ≤ b -> suc a ≤ suc b
≤-cong-suc .0 b z≤n = s≤s z≤n
≤-cong-suc .(suc _) .(suc _) (s≤s a<b) = s≤s (≤-cong-suc _ _ a<b) 

<-cong-suc : (a b : ℕ) → a < b → suc a < suc b
<-cong-suc a b a<b = ≤-cong-suc (suc a) b a<b

≤⇒<suc : (a b : ℕ) → a ≤ b → a < suc b
≤⇒<suc .0 b z≤n = s≤s z≤n
≤⇒<suc .(suc _) .(suc _) (s≤s {a} {b} a≤b) =  ≤-cong-suc (suc a) (suc b) (≤-cong-suc a b a≤b)

≡⇒<suc : {a : ℕ} → (b : ℕ) → a ≡ b → a < suc b
≡⇒<suc {.x} x refl = <-suc x

<-step : (a b : ℕ) → a < b → a < suc b
<-step a .(suc _) (s≤s a<b) = ≤-cong-suc a (suc _) (≤-step a<b)

<-suc⇒≤ : (a b : ℕ) → (a < suc b) → a ≤ b
<-suc⇒≤ a b (s≤s a≤b) = a≤b

<-zero-suc : (b : ℕ) → zero < suc b
<-zero-suc b = ≤⇒<suc zero b z≤n

<-suc-suc⇒< : (a b : ℕ) → suc a < suc b → a < b
<-suc-suc⇒< a (suc b) (s≤s {suc a} {suc b} sa≤sb) = sa≤sb

-- two proofs of x < k are equal
<-same-type⇒≡ : {a b : ℕ} → (p1 p2 : a < b) → p1 ≡ p2
<-same-type⇒≡ {zero} {.(suc _)} (s≤s z≤n) (s≤s z≤n) = refl
<-same-type⇒≡ {suc b} {.(suc _)} (s≤s p1) (s≤s p2) = cong s≤s $ <-same-type⇒≡ {b} {_} p1 p2

-- two proofs of suc x < suc k are equal 
<-same-sucs-type⇒≡ : (a b : ℕ) → (p1 p2 : suc a < suc b) → p1 ≡ p2
<-same-sucs-type⇒≡ a b p1 p2 = <-same-type⇒≡ {suc a} {suc b} p1 p2

-- iterated application of a function 
infixl 5 _$^_

_$^_ : {A : Set} → (f : A → A) → (n : ℕ) → (A → A)
(f $^ zero) a = a
(f $^ suc n) a = f $ (f $^ n) a

$^-additivity : {A : Set} → (f : A → A) → (m n : ℕ) → (a : A) → (λ a → (f $^ m) ((f $^ n) a)) a ≡ (f $^ (m + n)) a
$^-additivity f zero n a = refl
$^-additivity f (suc m) n a rewrite $^-additivity f m n a = refl

+-comm-+assoc : (x y z : ℕ) → (x + y) + z ≡ (y + z) + x
+-comm-+assoc x y z rewrite +-comm (y + z) x | +-assoc x y z = refl


