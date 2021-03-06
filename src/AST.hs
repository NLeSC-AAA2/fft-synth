-- ------ language="Haskell" file="src/AST.hs"
-- ------ begin <<ast-haskell-extensions>>[0]
{-# LANGUAGE GADTs,DataKinds,TypeOperators,KindSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances,UndecidableInstances #-}
-- ------ end

module AST where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

import Array
import Lib

-- ------ begin <<ast-ocl-namespaces>>[0]
data NameSpace = Static | Global | Constant
-- ------ end
-- ------ begin <<ast-basic-types>>[0]
class (Show a) => Declarable a where
  typename :: proxy a -> Text

data Pointer a = Pointer deriving (Show)
data Function :: [*] -> * -> * where
  Function :: Text -> Function a b
  deriving (Show)
newtype Variable a = Variable Text deriving (Show)

instance Declarable Int where
  typename _ = "int"
instance Declarable Double where
  typename _ = "R"
instance Declarable a => Declarable (Pointer a) where
  typename _ = typename (Proxy :: Proxy a) <> "*"
-- ------ end
-- ------ begin <<ast-typelists>>[0]
data ArgList :: [*] -> * where
    Empty :: ArgList '[]
    (:+:) :: Expr a -> ArgList l -> ArgList (a ': l)

--deriving instance Show (ArgList '[])
--deriving instance (Show (Expr a), Show (ArgList l)) => Show (ArgList (a ': l))
-- ------ end
-- ------ begin <<ast-expression>>[0]
data Expr a where
    Literal        :: (Show a) => a -> Expr a
    IntegerValue   :: Int -> Expr Int
    RealValue      :: Double -> Expr Double
    ArrayRef       :: Array a -> Expr (Array a)
    VarReference   :: Variable a -> Expr a
    ArrayIndex     :: Array a -> [Expr Int] -> Expr a
    TUnit          :: Expr ()
    Apply          :: Syntax (ArgList a) => Function a b -> ArgList a -> Expr b

infixr 1 :+:
-- ------ end
-- ------ begin <<ast-statements>>[0]
data Range = Range
    { start :: Int
    , end   :: Int
    , step  :: Int } deriving (Show)


data Stmt where
    VarDeclaration   :: (Declarable a, Show a) => Variable a -> Stmt
    Expression       :: Expr () -> Stmt
    ParallelFor      :: Variable Int -> Range -> [Stmt] -> Stmt
    Assignment       :: (Show a) => Variable a -> Expr a -> Stmt
    -- FunctionDef      :: Function a b -> [Stmt] -> [Stmt] -> Stmt
-- ------ end

data FunctionDecl a b = FunctionDecl
  { functionName :: Text
  , argNames     :: [Text]
  , functionBody :: [Stmt] }

-- ------ begin <<ast-syntax>>[0]
class Syntax a where
  generate :: a -> Text

instance Syntax (ArgList '[]) where
    generate _ = ""

instance (Syntax (Expr a), Syntax (ArgList l)) => Syntax (ArgList (a ': l)) where
    generate (a :+: Empty) = generate a
    generate (a :+: b) = generate a <> ", " <> generate b

instance Syntax (Expr a) where
  generate (Literal x) = tshow x
  generate (IntegerValue x) = tshow x
  generate (RealValue x) = tshow x
  generate (ArrayRef x)
    | (offset x) == 0 = name x
    | otherwise       = name x <> " + " <> tshow (offset x)
  generate (VarReference (Variable x)) = x
  generate (ArrayIndex a i) = name a <> "[<index expression>]"
  generate (Apply (Function f) a) = f <> "(" <> generate a <> ")"
  generate TUnit = ""

instance Syntax Stmt where
  generate (VarDeclaration v@(Variable x)) = typename v <> " " <> x <> ";"
  generate (Expression e) = generate e <> ";"
  generate (ParallelFor (Variable v) (Range a b s) body) =
    "for (int " <> v <> "=" <> tshow a <> ";" <> v <> "<"
    <> tshow b <> ";" <> v <> "+=" <> tshow s <> ") {\n"
    <> T.unlines (map generate body) <> "\n}"
  generate (Assignment (Variable v) e) = v <> " = " <> generate e <> ";"
-- ------ end
-- ------ end
