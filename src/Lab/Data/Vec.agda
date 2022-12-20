{-# OPTIONS --without-K #-}

module Lab.Data.Vec where

open import Lab.Prelude

open import Data.Vec.Base public


tabulateIO : ∀{A : Set} {n} → (Fin n → IO A) → IO $ Vec A n
tabulateIO {n = ℕ.zero}  f = pure []
tabulateIO {n = ℕ.suc n} f = do
    fx ← f Fin.zero
    fxs ← tabulateIO (f ∘ Fin.suc)
    pure $ fx ∷ fxs

iforM : ∀{aℓ bℓ n} {A : Set aℓ} {B : Set bℓ} {M : Set bℓ → Set bℓ} → ⦃ Monad M ⦄ → Vec A n → (Fin n → A → M B) → M (Vec B n)
iforM Vec.[]       f = return $ Vec.[]
iforM (x Vec.∷ xs) f = do
    y ← f Fin.zero x
    ys ← iforM xs (f ∘ Fin.suc)
    return $ y Vec.∷ ys

iforM- : ∀{A B : Set} {n} → Vec A n → (Fin n → A → IO B) → IO ⊤′
iforM- xs f = iforM xs f >> pure tt′
