{-# LANGUAGE TemplateHaskell #-}

import Data.Demote
import Language.Haskell.TH
import Demotest

lol = $(do
  TyConI (TySynD _ _ t) <- reify ''Lol
  case demote t of
    Just x -> stringE $ show $ (x :: MyType (Demoted Symbol) (Demoted Nat))
    Nothing -> fail "Failed to demote.")
