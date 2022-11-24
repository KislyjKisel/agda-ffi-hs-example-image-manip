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

forIO- : ∀{A B : Set} {n} → Vec A n → (Fin n → A → IO B) → IO ⊤′
forIO- []       f = pure tt′
forIO- (x ∷ xs) f = f Fin.zero x >> forIO- xs (λ i → f (Fin.suc i))
