-- ------ language="Haskell" file="src/AST.hs"
{-# LANGUAGE GADTs #-}

module AST where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Complex
import Array

newtype Function a b = Function Text
newtype Variable a = Variable Text
newtype Constant a = Constant Text

data Range = Range
    { start :: Int
    , end   :: Int
    , step  :: Int } deriving (Show)

data Expr a where
    IntegerValue   :: Int -> Expr Int
    RealValue      :: Double -> Expr Double
    ComplexValue   :: Complex Double -> Expr (Complex Double)
    VarReference   :: Variable a -> Expr a
    ConstReference :: Constant a -> Expr a
    ArrayIndex     :: Array a -> [Expr Int] -> Expr a
    FunctionCall   :: Function a b -> Expr b -> Expr a

data Stmt where
    VarDeclaration   :: Variable a -> Stmt
    ConstDeclaration :: Constant a -> Stmt
    Expression       :: Expr () -> Stmt
    ParallelFor      :: Variable Int -> Range -> [Stmt] -> Stmt
    Assignment       :: Variable a -> Expr a -> Stmt
-- ------ end
