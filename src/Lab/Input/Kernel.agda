{-# OPTIONS --without-K --allow-unsolved-metas #-}

module Lab.Input.Kernel where

open import Data.Nat.Divisibility                  using (_∣?_)
open import Data.Product                           using (_×_; _,_)
open import Relation.Nullary.Decidable.Core as Dec using ()

import Ffi.Hs.DearImGui as ImGui

open import Lab.Input   using (Input)
open import Lab.Prelude


kernel : (n : ℕ) → ⦃ Dec.False (2 ∣? n) ⦄ → Input
kernel n = record
    { State = IORef Float × Vec (Vec (IORef Float) n) n 
    ; Value = Float × Vec (Vec Float n) n
    ; new = do
        rs ← Vec.tabulateIO λ _ → Vec.tabulateIO λ _ → newIORef (f64⇒f32 0.0)
        k ← newIORef (f64⇒f32 1.0)
        pure $ k , rs
    ; load = λ (k , rs) → do
        rvs ← Vec.tabulateIO λ i → Vec.tabulateIO λ j → readIORef (Vec.lookup (Vec.lookup rs i) j)
        kv ← readIORef k
        pure $ kv , rvs
    ; ui = λ (k , rs) → do
        ImGui.dragFloat "Mult" k (f64⇒f32 0.01) (f64⇒f32 0.0) (f64⇒f32 2.0)
        Vec.forIO- rs λ i irs →
            Vec.forIO- irs λ j jr → do
                -- if (Int ∋ fromℕ $ Fin.toℕ j) == 0 then pure _ else ImGui.sameLine
                ImGui.dragFloat (Text.append (Text.append (sf i) (sf j)) "\0") jr (f64⇒f32 0.25) (f64⇒f32 0.0) (f64⇒f32 4.0)
    }
    where
    sf : ∀{n} → Fin n → Text
    sf = Text.pack ∘ show ∘ ((ℕ → Int) ∋ fromℕ) ∘ Fin.toℕ
