-- ------ language="Haskell" file="src/AST.hs"
{-# LANGUAGE GADTs #-}

module AST where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Complex

newtype Function a b = Function Text
newtype Variable a = Variable Text

data Expr a where
    IntegerValue :: Int -> Expr Int
    RealValue    :: Double -> Expr Double
    ComplexValue :: Complex Double -> Expr (Complex Double)
    Reference    :: Variable a -> Expr a
    FunctionCall :: Function a b -> Expr b -> Expr a
-- ------ end
