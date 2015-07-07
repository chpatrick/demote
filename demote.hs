{-# LANGUAGE FlexibleInstances, TypeOperators, PatternSynonyms, LambdaCase, TupleSections, DefaultSignatures, FlexibleContexts, DeriveAnyClass #-}

import Control.Applicative
import Control.Monad.State.Strict
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Generics

newtype DemotedSymbol = DemotedSymbol { getSymbol :: String }
  deriving (Eq, Ord, Show)

newtype DemotedNat = DemotedNat { getNat :: Integer }
  deriving (Eq, Ord, Show)

class Demotable a where
  demote :: Type -> Maybe a
  default demote :: (Generic a, DemotableG (Rep a)) => Type -> Maybe a
  demote = fmap to . demoteG

instance Demotable a => Demotable [ a ] where
  demote = \case
    PromotedNilT -> Just []
    PromotedConsT :$ x :$ xs -> (:) <$> demote x <*> demote xs
    _ -> Nothing

instance Demotable DemotedSymbol where
  demote = \case
    LitT (StrTyLit sym) -> Just (DemotedSymbol sym)
    _ -> Nothing

instance Demotable DemotedNat where
  demote = \case
    LitT (NumTyLit nat) -> Just (DemotedNat nat)
    _ -> Nothing

class DemotableG a where
  demoteG :: Type -> Maybe (a p)

class DemoteF a where
  demoteF :: StateT Type Maybe (a p)

pattern (:$) l r = AppT l r

instance (DemoteF f, DemoteF g) => DemoteF (f :*: g) where
  demoteF = do
    r <- demoteF 
    l <- demoteF
    return $ l :*: r

instance DemoteF U1 where
  demoteF = return U1

instance Demotable a => DemoteF (S1 c (Rec0 a)) where
  demoteF = StateT $ \case
    t' :$ vt -> do
      v <- demote vt
      return ( M1 $ K1 v, t' )
    _ -> Nothing

instance (Constructor c, DemoteF f) => DemotableG (C1 c f) where
  demoteG t = do
    ( st, ConT (Name (OccName occ) _) ) <- runStateT demoteF t
    r <- return (M1 st)
    guard (occ == conName r)
    return r

instance (DemotableG f, DemotableG g) => DemotableG (f :+: g) where
  demoteG t = L1 <$> demoteG t <|> R1 <$> demoteG t

instance DemotableG f => DemotableG (D1 c f) where
  demoteG = fmap M1 . demoteG

deriving instance Demotable ()
deriving instance Demotable Bool
deriving instance ( Demotable a, Demotable b ) => Demotable ( a, b )
deriving instance Demotable a => Demotable (Maybe a)