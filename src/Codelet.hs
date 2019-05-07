-- ------ language="Haskell" file="src/Codelet.hs"
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds,GeneralizedNewtypeDeriving #-}

import AST

module Codelet
  ( Codelet(..)
  , CodeletType(..)
  , codeletName ) where

import Data.Text (Text)
import Lib

data CodeletType = Twiddle | NoTwiddle deriving (Eq, Ord)

instance Show CodeletType where
  show Twiddle = "twiddle"
  show NoTwiddle = "notw"

data Codelet = Codelet
  { codeletType  :: CodeletType
  , codeletRadix :: Int }
  deriving (Eq, Show, Ord)

codeletName :: Codelet -> Text
codeletName Codelet{..} = tshow codeletType <> "_" <> tshow codeletRadix

type NoTwiddleCodelet a = Function
  [ Array a, Array a
  , Array a, Array a
  , Int, Int, Int, Int, Int ] ()

type TwiddleCodelet a = Function
  [ Array a, Array a
  , Array a
  , Int, Int, Int, Int ] ()
-- ------ end
