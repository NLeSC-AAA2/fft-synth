-- ------ language="Haskell" file="src/Codelet.hs"
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds,GeneralizedNewtypeDeriving #-}

module Codelet
  ( Codelet(..)
  , CodeletType(..)
  , codeletName
  , codeletPrefix
  , NoTwiddleCodelet
  , TwiddleCodelet ) where

import AST
import Array

import Data.Text (Text)
import Lib

data CodeletType = Twiddle | NoTwiddle deriving (Eq, Ord, Show)

data Codelet = Codelet
  { codeletType  :: CodeletType
  , codeletRadix :: Int }
  deriving (Eq, Show, Ord)

codeletPrefix :: Codelet -> Text
codeletPrefix Codelet{..} = case codeletType of
    Twiddle -> "twiddle"
    NoTwiddle -> "notw"

codeletName :: Codelet -> Text
codeletName x@Codelet{..} = codeletPrefix x <> "_" <> tshow codeletRadix

type NoTwiddleCodelet a = Function
  [ Array a, Array a
  , Array a, Array a
  , Int, Int, Int, Int, Int ] ()

type TwiddleCodelet a = Function
  [ Array a, Array a
  , Array a
  , Int, Int, Int, Int ] ()
-- ------ end
