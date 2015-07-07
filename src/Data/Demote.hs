{-# LANGUAGE FlexibleInstances, TypeOperators, PatternSynonyms, LambdaCase, TupleSections, DefaultSignatures, FlexibleContexts, DeriveAnyClass, StandaloneDeriving, ScopedTypeVariables #-}

module Data.Demote
  ( Demotable(..), demote
  , DemotedSymbol(..), DemotedNat(..)
  ) where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Generics

-- Demoted versions of TypeLits.
-- It's a good idea to parameterize your promoted types like so: MyType str
-- This way you can promote a MyType Symbol and demote it into a MyType DemotedSymbol and use the same data type.
newtype DemotedSymbol = DemotedSymbol { getSymbol :: String }
  deriving (Eq, Ord, Show)

newtype DemotedNat = DemotedNat { getNat :: Integer }
  deriving (Eq, Ord, Show)

class Demotable a where
  -- | Demote a Type that does not contain signatures.
  demote' :: Type -> Maybe a
  default demote' :: (Generic a, DemoteG (Rep a)) => Type -> Maybe a
  demote' = fmap to . demoteG

demote :: Demotable a => Type -> Maybe a
demote = demote' . desig
  where
    desig = \case
      SigT t _ -> desig t
      ForallT tvb cxt t -> ForallT tvb cxt (desig t)
      t `AppT` t' -> desig t `AppT` desig t'
      t -> t

class DemoteG a where
  demoteG :: Type -> Maybe (a p)

class DemoteF a where
  demoteF :: StateT Type Maybe (a p)

instance (DemoteF f, DemoteF g) => DemoteF (f :*: g) where
  demoteF = do
    r <- demoteF
    l <- demoteF
    return $ l :*: r

instance DemoteF U1 where
  demoteF = return U1

instance Demotable a => DemoteF (S1 c (Rec0 a)) where
  demoteF = StateT $ \case
    t' `AppT` vt -> do
      v <- demote' vt
      return ( M1 $ K1 v, t' )
    _ -> Nothing

instance (Constructor c, DemoteF f) => DemoteG (C1 c f) where
  demoteG t = do
    ( x, ConT (Name (OccName occ) _) ) <- runStateT demoteF t
    case M1 x of
      r | occ == conName r -> Just r
      _ -> Nothing

instance (DemoteG f, DemoteG g) => DemoteG (f :+: g) where
  demoteG t = L1 <$> demoteG t <|> R1 <$> demoteG t

instance DemoteG f => DemoteG (D1 c f) where
  demoteG = fmap M1 . demoteG

instance Demotable a => Demotable [ a ] where
  demote' = \case
    PromotedNilT -> Just []
    PromotedConsT `AppT` x `AppT` xs -> (:) <$> demote' x <*> demote' xs
    _ -> Nothing

instance Demotable DemotedSymbol where
  demote' = \case
    LitT (StrTyLit sym) -> Just (DemotedSymbol sym)
    _ -> Nothing

instance Demotable DemotedNat where
  demote' = \case
    LitT (NumTyLit nat) -> Just (DemotedNat nat)
    _ -> Nothing

deriving instance Demotable ()
deriving instance Demotable Bool
deriving instance ( Demotable a, Demotable b ) => Demotable ( a, b )
deriving instance ( Demotable a, Demotable b, Demotable c ) => Demotable ( a, b, c )
deriving instance Demotable a => Demotable (Maybe a)
