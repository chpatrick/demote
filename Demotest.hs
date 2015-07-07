{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds #-}

module Demotest where

import Data.Demote
import GHC.Generics
import GHC.TypeLits

data MyType str nat = MyType { templateSources :: [ ( str, nat ) ] }
  deriving (Generic, Demotable, Show)

type Lol = 'MyType '[ '( "foo", 42 ) ]