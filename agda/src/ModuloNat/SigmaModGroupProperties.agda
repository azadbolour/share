
module SigmaModGroupProperties where

open import Data.Nat using (ℕ; zero; suc; pred; _<_; _≤_; s≤s; z≤n; _+_; _∸_)
open import Data.Nat.Properties using (+-assoc; +-comm; n≤1+n; <⇒≤; m+n∸m≡n; n∸m≤n; m∸n+n≡m) 
open import Data.Product using (_,_)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; _≢_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _∎; _≡⟨⟩_; _≡⟨_⟩_)
open import UtilProperties using (≤⇒<suc; <-suc-suc⇒<; +-comm-+assoc)
open import SigmaMod using (Mod; zeroₘ; sucₘ; sucₘⁿ; _+ₘ_; -ₘ_; +ₘ-by-+; naturalₘ; 
  sucₘⁿ-additivity ; +ₘ-comm; sucₘⁿ-zeroₘ; cycle-zeroₘ )

-- group left identity law for +ₘ
+ₘ-identityˡ : {k : ℕ} → (mod : Mod k) → zeroₘ +ₘ mod ≡ mod
+ₘ-identityˡ {k} mod = begin zeroₘ +ₘ mod ≡⟨⟩ sucₘⁿ 0 mod ≡⟨⟩ mod ∎

-- group right identity law for +ₘ
+ₘ-identityʳ : {k : ℕ} → (mod : Mod k) → mod +ₘ zeroₘ ≡ mod
+ₘ-identityʳ {k} mod rewrite +ₘ-comm mod zeroₘ | +ₘ-identityˡ mod = refl  

-- variant of +/∸ cancellation theorem
sm<sn⇒m+n∸m≡n : {k : ℕ} → (x : ℕ) → (sx<sk : suc x < suc k) → x + (k ∸ x) ≡ k
sm<sn⇒m+n∸m≡n {k} x sx≤sk = let x<k = <-suc-suc⇒< x k sx≤sk in m+n∸m≡n {x} {k} (<⇒≤ x<k)

-- variant of +/∸ cancellation theorem
sm<sn⇒n∸m+sm≡sn : {k : ℕ} → (x : ℕ) → (sx<sk : suc x < suc k) → (k ∸ x) + suc x ≡ suc k
sm<sn⇒n∸m+sm≡sn  {k} x sx<sk rewrite +-comm (k ∸ x) (suc x) | sm<sn⇒m+n∸m≡n {k} x sx<sk = refl 

-- group left inverse law for +ₘ
+ₘ-inverseˡ : {k : ℕ} → (mod : Mod k) → ( -ₘ mod ) +ₘ mod ≡ zeroₘ
+ₘ-inverseˡ {k} (zero , s≤s z≤n) = refl
+ₘ-inverseˡ {k} (suc x , sx<sk) =
  let k∸x<sk = ≤⇒<suc (k ∸ x ) k (n∸m≤n x k)
  in
  begin
    (k ∸ x , k∸x<sk) +ₘ (suc x , sx<sk) ≡⟨ +ₘ-by-+ (k ∸ x , k∸x<sk) (suc x , sx<sk) ⟩
    sucₘⁿ-zeroₘ ((k ∸ x) + suc x) ≡⟨ cong sucₘⁿ-zeroₘ (sm<sn⇒n∸m+sm≡sn {k} x sx<sk) ⟩
    sucₘⁿ-zeroₘ (suc k) ≡⟨ cycle-zeroₘ {k} ⟩
    zeroₘ
  ∎

-- group right inverse law for +ₘ
+ₘ-inverseʳ : {k : ℕ} → (mod : Mod k) → mod +ₘ ( -ₘ mod ) ≡ zeroₘ
+ₘ-inverseʳ {k} mod rewrite +ₘ-comm mod ( -ₘ mod ) | +ₘ-inverseˡ mod  = refl

-- modulo plus applied twice is a function of natural number plus applied twice
+ₘ-+ₘ-by-+-+ : {k : ℕ} → (mx my mz : Mod k) → (mx +ₘ my) +ₘ mz ≡ sucₘⁿ-zeroₘ ((naturalₘ mx + naturalₘ my) + naturalₘ mz)
+ₘ-+ₘ-by-+-+  {k} mx@(x , _) my@(y , _) mz@(z , _) =
  begin
    (mx +ₘ my) +ₘ mz ≡⟨ cong (_+ₘ mz) (+ₘ-by-+ mx my)  ⟩
    sucₘⁿ-zeroₘ (x + y) +ₘ mz ≡⟨ +ₘ-comm (sucₘⁿ-zeroₘ (x + y)) mz ⟩
    mz +ₘ sucₘⁿ-zeroₘ (x + y) ≡⟨⟩
    sucₘⁿ z (sucₘⁿ (x + y) zeroₘ) ≡⟨ sucₘⁿ-additivity z (x + y) zeroₘ ⟩
    sucₘⁿ-zeroₘ (z + (x + y))  ≡⟨ cong sucₘⁿ-zeroₘ (+-comm z (x + y)) ⟩
    sucₘⁿ-zeroₘ ((x + y) + z) 
  ∎

-- group associativity law for +ₘ
+ₘ-assoc : {k : ℕ} → (mx my mz : Mod k) → (mx +ₘ my) +ₘ mz ≡ mx +ₘ (my +ₘ mz)
+ₘ-assoc {k} mx@(x , _) my@(y , _) mz@(z , _) =
  begin
    (mx +ₘ my) +ₘ mz ≡⟨ +ₘ-+ₘ-by-+-+ mx my mz ⟩
    sucₘⁿ-zeroₘ ((x + y) + z) ≡⟨ cong sucₘⁿ-zeroₘ (+-comm-+assoc x y z) ⟩
    sucₘⁿ-zeroₘ ((y + z) + x) ≡⟨ sym (+ₘ-+ₘ-by-+-+ my mz mx) ⟩
    (my +ₘ mz) +ₘ mx ≡⟨ +ₘ-comm (my +ₘ mz) mx ⟩
    mx +ₘ (my +ₘ mz)  
  ∎

record Group {ℓ} (A : Set ℓ) : Set ℓ where
  field
    -- Definitions.
    _✴_ : A → A → A                                           -- group operation (\st5)
    ✴-id : A                                                  -- identity element
    ✴- : A → A                                                -- inverse
    -- Properties.
    ✴-identityˡ : (a : A) → ✴-id ✴ a ≡ a                      -- left identity 
    ✴-identityʳ : (a : A) → a ✴ ✴-id ≡ a                      -- right identity
    ✴-inverseˡ : (a : A) → (✴- a) ✴ a ≡ ✴-id                  -- left inverse
    ✴-inverseʳ : (a : A) → a ✴ (✴- a) ≡ ✴-id                  -- right inverse
    ✴-assoc : (x y z : A) → (x ✴ y) ✴ z ≡ x ✴ (y ✴ z)         -- associativity

instance
  ModGroup : {k : ℕ} → Group (Mod k)
  ModGroup = record {
    _✴_ = _+ₘ_ ;
    ✴-id = zeroₘ ;
    ✴- = -ₘ_ ;
    ✴-identityˡ = +ₘ-identityˡ ;
    ✴-identityʳ = +ₘ-identityʳ ;
    ✴-inverseˡ = +ₘ-inverseˡ ;
    ✴-inverseʳ = +ₘ-inverseʳ ;
    ✴-assoc = +ₘ-assoc 
   }
    
-- TODO. Example use of instance argument for group. 
