---
title: FFT Synthesis
---

# Array arithmetic

We will express different stages of the FFT by applying a vectorised FFT on a multi-dimensional strided numeric array.

``` {.haskell file=src/Array.hs}
{-# LANGUAGE DuplicateRecordFields #-}

module Array where

import Data.Complex (Complex(..))
import Data.Proxy

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Except

import Lib


<<array-numeric-class>>
<<array-types>>
<<array-methods>>
```

## Numeric types

The `NumericType` type-class is defined so that we can constrain functions to work with `Float`, `Double` or `Complex a`. This also gives information on the byte size of each numeric type. We need to give `byteSize` an argument for it to be able to deduce the type.

``` {.haskell #array-numeric-class}
class NumericType a where
    byteSize :: proxy a -> Int

instance NumericType Float where
    byteSize _ = 4

instance NumericType Double where
    byteSize _ = 8

instance (NumericType a) => NumericType (Complex a) where
    byteSize _ = 2 * (byteSize (Proxy :: Proxy a))
```

## Array structure

An array has a shape, stride and offset, in addition to a name meant to identify the array in question.

``` {.haskell #array-types}
type Shape = [Int]
type Stride = [Int]

data Array a = Array
    { name     :: Text
    , shape    :: Shape
    , stride   :: Stride
    , offset   :: Int } deriving (Show)

-- data ArrayIndex a = ArrayIndex
--    { name     :: Text
--     , index    :: Int } deriving (Show)

floatArray :: Text -> Shape -> Array Float
floatArray name shape = Array name shape (fromShape shape 1) 0
```

The stride gives the distance in memory (counted in number of items, not bytes) for a step in each axis of the array. The location in memory can be computed using the dot-product,

$${\rm location} = {\rm stride} \cdot {\rm index} + {\rm offset}.$$ {#eq:strides}

## Array methods

Given a contiguous array of a given shape, we can compute  the stride by taking the cumulative product.

``` {.haskell #array-methods}
fromShape :: Shape -> Int -> Stride
fromShape [] _ = []
fromShape (x:xs) n = n : fromShape xs (n * x)
```

### Properties

Basic properties of an array: size, dimension and if the array is contiguous.

``` {.haskell #array-methods}
ndim :: Array a -> Int
ndim = length . shape

contiguous :: Array a -> Bool
contiguous Array{shape,stride} = stride == fromShape shape 1
```

### Error handling

Methods that can fail will return a type `Either Text a`. Since that type implements the `MonadError` class we can throw errors using `throwError` and at success `return` a value. We can generalize this pattern by accepting just any `MonadError Text` type.

``` {.haskell #array-methods}
rcheck :: MonadError Text m => Text -> Int -> Int -> m ()
rcheck what n i
    | (i >= 0) && (i < n) = return ()
    | otherwise = throwError $ "Range check error: " <> what <> " "
                      <> tshow n <> " " <> tshow i
```

The `rcheck` function implements a range-check on the range `0 .. (n-1)`.

### Reshaping, slicing

If we have an array of complex values, we want to read out the real and imaginary parts.

```{.haskell #array-methods}
realPart :: Array (Complex a) -> Array a
realPart array@Array{stride} = array
    { stride = map (* 2) stride }

imagPart :: Array (Complex a) -> Array a
imagPart array@Array{stride, offset} = array
    { stride = map (* 2) stride
    , offset = offset + 1 }
```

Transposing an array means reversing the shape and stride vectors

``` {.haskell #array-methods}
transpose :: Array a -> Array a
transpose array@Array{shape, stride} = array
    { shape = reverse shape
    , stride = reverse stride }
```

Reshaping is only possible from a contiguous array. Otherwise the arithmetic of stepping through the resulting array would no longer be expressible in terms of a stride and offset.

``` {.haskell #array-methods}
reshape :: MonadError Text m => Shape -> Array a -> m (Array a)
reshape newShape array
    | contiguous array = return $ array
        { shape = newShape
        , stride = fromShape newShape 1 }
    | otherwise = throwError "Cannot reshape non-contiguous array."
```

The `select`, `extrude`, and `slice` methods do the same as the Numpy array slice notation.

| Numpy          | Function          | Description           |
| -------------- | ----------------- | --------------------- |
| `a[:,3]`       | `select a 1 3`    | Select 4th column     |
| `a[3:9:2,:]`   | `slice a 0 3 9 2` | Slice rows 4, 6 and 8 |
| `a[:,None,:]`  | `extrude a 1`     | Extrude a new axis    |

``` {.haskell #array-methods}
select :: MonadError Text m => Int -> Int -> Array a -> m (Array a)
select dim i array@Array{shape,stride,offset} = do
    rcheck "dim" (ndim array) dim
    rcheck "size" (shape !! dim) i
    return $ array
        { shape  = remove shape dim
        , stride = remove stride dim
        , offset = offset + (stride !! dim) * i }

extrude :: MonadError Text m => Int -> Array a -> m (Array a)
extrude dim array@Array{shape,stride} = do
    rcheck "dim" (ndim array + 1) dim
    return $ array
        { shape  = insert shape dim 1
        , stride = insert stride dim ((stride !! dim) * (shape !! dim)) }

slice :: MonadError Text m => Int -> Int -> Int -> Int -> Array a -> m (Array a)
slice dim a b step array@Array{shape,stride,offset} = do
    rcheck "dim" (ndim array) dim
    rcheck "a" ((shape !! dim) + 1) a
    rcheck "b" ((shape !! dim) + 1) b
    return $ array
        { shape  = replace shape  dim ((b - a) `quot` step)
        , stride = replace stride dim ((stride !! dim) * step)
        , offset = offset + (stride !! dim) * a }
```

# Codelets

A codelet, for the moment, is just some function that we can call.

``` {.haskell file=src/Codelet.hs}
module Codelet where

import Data.Text (Text)
import qualified Data.Text as T

import Lib
import AST

data Codelet = Codelet
    { prefix  :: Text
    , radix   :: Int }

codeletName :: Codelet -> Text
codeletName c = prefix c <> "_" <> tshow (radix c)
```

# Twiddle factors

In Python we created an array of twiddle factors:

``` {.python}
def w(k, n):
    return np.exp(2j * np.pi * k / n)

def make_twiddle(n1, n2):
    I1 = np.arange(n1)
    I2 = np.arange(n2)
    return w(I1[:,None] * I2[None,:], n1*n2).astype('complex64')
```

In Haskell this is a bit different:

``` {.haskell file=src/TwiddleFactors.hs}
module TwiddleFactors where

import Data.Complex

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Array

<<twiddle-factors-w>>
<<twiddle-factors-multi-w>>
<<twiddle-factors-make>>
```

We still have the equation

$$w_n^k = \exp \left[2 \pi i \frac{k}{n}\right].$$

Haskell provides the function `cis` which computes a complex point on the unit circle given the phase.

``` {.haskell #twiddle-factors-w}
w :: RealFloat a => Int -> Int -> Complex a
w n k = cis (2 * pi * fromIntegral k / fromIntegral n)
```

We need to map the index vector to a value $w_{\prod n_i}^{\prod k_i}$.

``` {.haskell #twiddle-factors-multi-w}
multiW :: RealFloat a => Shape -> [Int] -> Complex a
multiW n k = w (product n) (product k)
```

To generate the list of indices (lists are lazy, so this should be efficient enough), we have a nifty one-liner. Given a list of indices for a reduced shape vector we can create the full list by prepending numbers from the range `[0 .. n-1]`.

``` {.haskell #twiddle-factors-make}
indices :: Shape -> [[Int]]
indices = foldr (\ n -> concatMap (\ idcs -> map (: idcs) [0 .. n-1])) [[]]

makeTwiddle :: Shape -> Vector (Complex Double)
makeTwiddle shape = V.fromList $ map (multiW shape) $ indices shape
```

## Unit tests

``` {.haskell #test-predicates}
class Approx a where
    closeTo :: a -> a -> Bool

instance Approx Float where
    closeTo a b = abs (a - b) < 1e-5

instance Approx Double where
    closeTo a b = abs (a - b) < 1e-10

instance (Approx a, Applicative m, Foldable m) => Approx (m a) where
    closeTo x y = and $ liftA2 closeTo x y

instance {-# OVERLAPPING #-} (Approx a, V.Unbox a) => Approx (Vector a) where
    closeTo x y = V.and $ V.zipWith closeTo x y
```

```{.haskell #test-twiddle-factors}
describe "TwiddleFactors.indices" $
    it "creates an index list" $ do
        indices [2, 2] `shouldBe` [[0, 0], [1, 0], [0, 1], [1, 1]]
        indices [3, 1] `shouldBe` [[0, 0], [1, 0], [2, 0]]

describe "TwiddleFactors.makeTwiddle" $
    it "Generates twiddle factors" $ do
        makeTwiddle [2, 2] `shouldSatisfy` closeTo
            (V.fromList [ 1.0, 1.0, 1.0, 0.0 :+ 1.0 ])
        makeTwiddle [4] `shouldSatisfy` closeTo
            (V.fromList [ 1.0, 0.0 :+ 1.0, -1.0, 0.0 :+ (-1.0) ])
```

# Abstract Syntax Tree

``` {.haskell file=src/AST.hs}
{-# LANGUAGE GADTs,DataKinds,TypeOperators,KindSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances,UndecidableInstances #-}

module AST where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

-- import Data.Complex
import Array
import Lib

class (Show a) => Declarable a where
  typename :: proxy a -> Text

data Pointer a = Pointer deriving (Show)
data Function :: [*] -> * -> * where
  Function :: Text -> Function a b
  deriving (Show)
newtype Variable a = Variable Text deriving (Show)
newtype Constant a = Constant Text deriving (Show)

instance Declarable Int where
  typename _ = "int"
instance Declarable Double where
  typename _ = "R"
instance Declarable a => Declarable (Pointer a) where
  typename _ = typename (Proxy :: Proxy a) <> "*"

data Range = Range
    { start :: Int
    , end   :: Int
    , step  :: Int } deriving (Show)

data HList :: [*] -> * where
    Nil :: HList '[]
    Cons :: a -> HList l -> HList (a ': l)

instance Show (HList '[]) where
    show Nil = "Nil"

instance (Show a, Show (HList l)) => Show (HList (a ': l)) where
    show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"
    
data Expr a where
    Literal        :: (Show a) => a -> Expr a
    IntegerValue   :: Int -> Expr Int
    RealValue      :: Double -> Expr Double
    -- ComplexValue   :: Complex Double -> Expr (Complex Double)
    ArrayRef       :: Array a -> Expr (Array a)
    VarReference   :: Variable a -> Expr a
    ConstReference :: Constant a -> Expr a
    ArrayIndex     :: Array a -> [Expr Int] -> Expr a
    TUnit          :: Expr ()
    TNull          :: Expr (HList '[])
    (:+:)          :: (Show a) => Expr a -> Expr (HList b) -> Expr (HList (a ': b))
    Apply          :: Function a b -> Expr (HList a) -> Expr b

infixr 1 :+:

-- deriving instance Show a => Show (Expr a)

data Stmt where
    VarDeclaration   :: (Declarable a, Show a) => Variable a -> Stmt
    ConstDeclaration :: (Declarable a, Show a) => Constant a -> Stmt
    Expression       :: Expr () -> Stmt
    ParallelFor      :: Variable Int -> Range -> [Stmt] -> Stmt
    Assignment       :: (Show a) => Variable a -> Expr a -> Stmt

-- deriving instance Show Stmt

data FunctionDecl a b = FunctionDecl
  { functionName :: Text
  , argNames     :: [Text]
  , functionBody :: [Stmt] }

class Syntax a where
  generate :: a -> Text

instance Syntax (Expr a) where
  generate (Literal x) = tshow x
  generate (IntegerValue x) = tshow x
  generate (RealValue x) = tshow x
  generate (ArrayRef x) = name x <> " + " <> tshow (offset x)
  generate (VarReference (Variable x)) = x
  generate (ConstReference (Constant x)) = x
  generate (ArrayIndex a i) = name a <> "[<index expression>]"
  generate (Apply (Function f) a) = f <> "(" <> generate a <> ")"
  generate (a :+: TNull) = generate a
  generate (a :+: b) = generate a <> ", " <> generate b
  generate TNull = ""

instance Syntax Stmt where
  generate (VarDeclaration v@(Variable x)) = typename v <> " " <> x <> ";"
  generate (ConstDeclaration v@(Constant x)) = typename v <> " const " <> x <> ";"
  generate (Expression e) = generate e <> ";"
  generate (ParallelFor (Variable v) (Range a b s) body) =
    "for (int " <> v <> "=" <> tshow a <> ";" <> v <> "<"
    <> tshow b <> ";" <> v <> "+=" <> tshow s <> ") {\n"
    <> T.unlines (map generate body) <> "\n}"
  generate (Assignment (Variable v) e) = v <> " = " <> generate e <> ";"
```

# Calling GenFFT

```{.haskell file=src/GenFFT.hs}
{-# LANGUAGE RecordWildCards #-}

module GenFFT where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process
import Data.Maybe

import Lib (tshow)

class ValidArg a where
  toText :: a -> Text

instance ValidArg String where
  toText = T.pack

instance ValidArg Text where
  toText = id

instance ValidArg Int where
  toText = tshow

instance ValidArg Double where
  toText = tshow

macros :: [(Text, Text)]
macros =
  [ ("R","float")
  , ("E","R")
  , ("stride","int")
  , ("INT","int")
  , ("K(x)","((E) x)")
  , ("DK(name,value)","const E name = K(value)")
  , ("WS(s,i)","s*i")
  , ("MAKE_VOLATILE_STRIDE(x,y)","0")
  , ("FMA(a,b,c)","a * b + c")
  , ("FMS(a,b,c)","a * b - c")
  , ("FNMA(a,b,c)","-a * b - c")
  , ("FNMS(a,b,c)","-a * b + c") ]

genMacros :: Text
genMacros = T.unlines $ map (\(k, v) -> "#define " <> k <> " " <> v) macros

genFFT :: Text -> [Text] -> IO Text
genFFT kind args = do
  let progName = T.unpack $ "./genfft/gen_" <> kind <> ".native"
      -- spec = RawCommand progName (map T.unpack args)
  code <- T.pack <$> readProcess progName (map T.unpack args) ""
  indent code

indent :: Text -> IO Text
indent x = T.pack <$> readProcess "indent" ["-nut"] (T.unpack x)

data GenFFTArgs = GenFFTArgs
  { compact     :: Bool
  , standalone  :: Bool
  , opencl      :: Bool
  , name        :: Maybe Text } deriving (Show)

defaultArgs :: GenFFTArgs
defaultArgs = GenFFTArgs
  { compact = True
  , standalone = True
  , opencl = True
  , name = Nothing }

optionalArg :: (ValidArg a) => Text -> Maybe a -> [Text]
optionalArg opt val = fromMaybe [] (do {n <- val; return [opt, toText n]})

argList :: GenFFTArgs -> [Text]
argList GenFFTArgs{..} =
  optionalArg "-name" name
  <> ["-compact" | compact]
  <> ["-standalone" | standalone]
  <> ["-opencl" | opencl]

genNoTwiddle :: Int -> GenFFTArgs -> IO Text
genNoTwiddle radix args = do
  let name' = fromMaybe ("notw_" <> tshow radix) (name args)
  genFFT "notw" $ ["-n", tshow radix] <> argList args{name=Just name'}

genTwiddle :: Int -> GenFFTArgs -> IO Text
genTwiddle radix args = do
  let name' = fromMaybe ("twiddle_" <> tshow radix) (name args)
  genFFT "twiddle" $ ["-n", tshow radix] <> argList args{name=Just name'}
```

# Generating larger FFT

``` {.haskell file=src/Synthesis.hs}
{-# LANGUAGE DataKinds,GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Synthesis where

import Data.Complex (Complex(..))
import Data.Text (Text)

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.List (sort)

import AST
import Array
import Lib
import Control.Monad.Except

import Math.NumberTheory.Primes.Factorisation (factorise)

type NoTwiddleCodelet a = Function
  [ Array a, Array a
  , Array a, Array a
  , Int, Int, Int, Int, Int ] ()

type TwiddleCodelet a = Function
  [ Array a, Array a
  , Array a
  , Int, Int, Int, Int ] ()

(!?) :: MonadError Text m => [a] -> Int -> m a
[] !? _ = throwError "List index error."
(x:xs) !? i
  | i == 0    = return x
  | otherwise = xs !? (i - 1)

planNoTwiddle :: (MonadError Text m, RealFloat a) => NoTwiddleCodelet a -> Array (Complex a) -> Array (Complex a) -> m (Expr ())
planNoTwiddle f inp out = do
  is  <- stride inp !? 0
  os  <- stride out !? 0
  v   <- shape inp !? 1
  ivs <- stride inp !? 1
  ovs <- stride out !? 1
  return $ Apply f (ArrayRef (realPart inp) :+: ArrayRef (imagPart inp) :+:
                    ArrayRef (realPart out) :+: ArrayRef (imagPart out) :+:
                    Literal is :+: Literal os :+: Literal v :+: Literal ivs :+: Literal ovs :+: TNull)

planTwiddle :: (MonadError Text m, RealFloat a) => TwiddleCodelet a -> Array (Complex a) -> Array (Complex a) -> m (Expr ())
planTwiddle f inp twiddle = do
  rs  <- stride inp !? 0
  me  <- shape inp !? 1
  ms  <- stride inp !? 1
  return $ Apply f (ArrayRef (realPart inp) :+: ArrayRef (imagPart inp) :+:
                    ArrayRef (realPart twiddle) :+: Literal rs :+: Literal 0 :+: Literal me :+: Literal ms :+: TNull)

data Codelet = Codelet
  { codeletPrefix :: Text
  , codeletRadix  :: Int }
  deriving (Eq, Show, Ord)

codeletName :: Codelet -> Text
codeletName Codelet{..} = codeletPrefix <> "_" <> tshow codeletRadix

factorsName :: Shape -> Text
factorsName s = "w_" <> T.intercalate "_" (map tshow s)

newtype Algorithm = Algorithm (Set Codelet, Set Shape, [Stmt])
  deriving (Semigroup, Monoid)

-- instance (Monad m) => Semigroup (m Algorithm) where
--   a <> b = do
--     a' <- a
--     b' <- b
--     return (a' <> b')

instance (Monad m, Semigroup (m Algorithm)) => Monoid (m Algorithm) where
  mempty = return mempty

noTwiddleFFT :: (MonadError Text m, RealFloat a) => Int -> Array (Complex a) -> Array (Complex a) -> m Algorithm
noTwiddleFFT n inp out = do
  let codelet = Codelet "notw" n
      f = Function (codeletName codelet) :: NoTwiddleCodelet a
  plan <- planNoTwiddle f inp out
  return $ Algorithm (S.fromList [codelet], mempty, [Expression plan])

twiddleFFT :: (MonadError Text m, RealFloat a) => Int -> Array (Complex a) -> Array (Complex a) -> m Algorithm
twiddleFFT n inp twiddle = do
  let codelet = Codelet "twiddle" n
      f = Function (codeletName codelet) :: TwiddleCodelet a
  plan <- planTwiddle f inp twiddle
  return $ Algorithm (S.fromList [codelet], mempty, [Expression plan])

-- nFactorFFT :: (MonadError Text m, RealFloat a) => [Int] -> Array (Complex a) -> Array (Complex a) -> m Algorithm
nFactorFFT :: RealFloat a => [Int] -> Array (Complex a) -> Array (Complex a) -> Either Text Algorithm
nFactorFFT [] _ _         = return mempty
nFactorFFT [x] inp out    = noTwiddleFFT x inp out
nFactorFFT (x:xs) inp out = do
  let n = product xs
      w_array = Array (factorsName [n, x]) [n, x] (fromShape [n, x] 1) 0
      subfft i = do {
        inp' <- reshape [n, x] =<< select 1 i inp;
        out' <- reshape [x, n] =<< select 1 i out;
        (nFactorFFT xs (transpose inp') out') <> (twiddleFFT x (transpose out') w_array) } :: Either Text Algorithm

  l <- shape inp !? 1
  mconcat (map subfft [0..l])

factors :: Int -> [Int]
factors n = sort $ concatMap (\(i, m) -> take m $ repeat (fromIntegral i :: Int)) (factorise $ fromIntegral n)

fullFactorFFT :: RealFloat a => Int -> Array (Complex a) -> Array (Complex a) -> Either Text Algorithm
fullFactorFFT n = nFactorFFT (factors n)
```

# Appendix: Miscellaneous functions

``` {.haskell file=src/Lib.hs}
module Lib where

import Data.Text (Text)
import qualified Data.Text as T

<<lib-list-manipulation>>

tshow :: Show a => a -> Text
tshow = T.pack . show
```

## List manipulation

We will be manipulating the shape and stride lists to slice the n-dimensional array. These operations just didn't make it into Haskell's standard library.

```{.haskell #lib-list-manipulation}
remove :: [a] -> Int -> [a]
remove [] _ = []
remove (x:xs) n
    | n == 0 = xs
    | otherwise = x : remove xs (n - 1)

replace :: [a] -> Int -> a -> [a]
replace [] _  _ = []
replace (x:xs) n y
    | n == 0 = y:xs
    | otherwise = x : replace xs (n - 1) y

insert :: [a] -> Int -> a -> [a]
insert [] _  _ = []
insert (x:xs) n y
    | n == 0 = y:x:xs
    | otherwise = x : insert xs (n - 1) y
```

# Unit tests

``` {.haskell file=test/Spec.hs}
{-# LANGUAGE DuplicateRecordFields #-}

import Test.Hspec
import Data.Complex
import Control.Applicative (liftA2)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Array
import Lib
import TwiddleFactors

<<test-predicates>>

testTwiddleFactors :: Spec
testTwiddleFactors = do
    <<test-twiddle-factors>>

main :: IO ()
main = hspec $ do
    describe "Strides.fromShape" $
        it "computes strides from shapes" $ do
            fromShape [3, 3, 3] 1 `shouldBe` [1, 3, 9]
            fromShape [2, 3, 5] 1 `shouldBe` [1, 2, 6]

    describe "Strides.remove" $
        it "drops indexed entry from list" $ do
            remove [1, 2, 3, 4 :: Int] 0 `shouldBe` [2, 3, 4]
            remove [1, 2, 3, 4 :: Int] 2 `shouldBe` [1, 2, 4]

    describe "Strides.replace" $
        it "replaces entry at index" $ do
            replace [1, 2, 3, 4 :: Int] 0 7 `shouldBe` [7, 2, 3, 4]
            replace [1, 2, 3, 4 :: Int] 2 7 `shouldBe` [1, 2, 7, 4]

    describe "Strides.insert" $
        it "inserts entry at index" $ do
            insert [1, 2, 3, 4 :: Int] 0 7 `shouldBe` [7, 1, 2, 3, 4]
            insert [1, 2, 3, 4 :: Int] 2 7 `shouldBe` [1, 2, 7, 3, 4]

    let a1 = floatArray "test" [4, 5]
    describe "Strides.select" $
        it "selects sub-array" $ do
            let a103 = select a1 0 3
            let a112 = select a1 1 2
            (shape <$> a103) `shouldBe` Right [5]
            (stride <$> a103) `shouldBe` Right [4]
            (offset <$> a103) `shouldBe` Right 3
            (shape <$> a112) `shouldBe` Right [4]
            (stride <$> a112) `shouldBe` Right [1]
            (offset <$> a112) `shouldBe` Right 8

    testTwiddleFactors
```

