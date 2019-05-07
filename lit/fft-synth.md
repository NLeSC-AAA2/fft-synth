---
title: FFT Synthesis
---

# Cooley-Tukey algorithm

(sampled from Frigo 1999. I changed array indices to $k, l, m$, $n$ denoting the length of the array.)

The (forward) discrete Fourier transform of $X$ is the array $Y$ given by

$$Y[l] = \sum_{k=0}^{n-1} X[k] w_n^{-kl},$$ {#eq:dft}

where 

$$w_n = \exp\left(\frac{2\pi i}{n}\right).$$ {#eq:wn}

So $w_n^{-kl}$ denotes $w_n$ *to the power of* $-kl$.

In the case that $X$ is real valued, $Y$ will have *hermitian symmetry*

$$Y[n - k] = Y^*[k].$$ {#eq:hermitian-symmetry}

The backward DFT flips the sign at the exponent of $w_n$, giving

$$Y[l] = \sum_{k=0}^{n-1} X[k] w_n^{kl}.$$ {#eq:idft}

Now suppose that $n$ can be factored into $n = n_1 n_2$. We may now view the arrays $X$ and $Y$ in a rectangular shape, where

$$X[k] = X[k_1 n_2 + k_2] = X[k_1, k_2].$$

Also, let $Y$ take the transposed shape of $X$,

$$Y[l] = Y[l_1 + l_2 n_1] = Y[l_1, l_2].$$

Then +@eq:dft can be written as

$$Y[l_1, l_2] = \sum_{k_2 = 0}^{n_2 - 1} \left[\left(\sum_{k_1 = 0}^{n_1-1} X[k_1, k_2] w_{n_1}^{-k_1 l_1}\right) w_n^{l_1 k_2}\right] w_{n_2}^{-l_2 k_2}.$$

Also known as the **Cooley-Tukey fast Fourier transform**.

This separates the DFT in an inner and outer transform, of sizes $n_1$ and $n_2$. The output of the inner transform is multiplied by **twiddle factors** $w_n^{-l_1 k_2}$, before the outer transform is done.

## Array arithmetic

![Cooley-Tukey algorithm for an $n=20$ array.](cooley-tukey-20.svg){#fig:cooley-tukey-array}

We will express different stages of the FFT by applying a vectorised FFT on a multi-dimensional strided numeric array. In the following example we compute an $n=20$ transform by doing four radix-5 transforms followed by five $2 \times 2$ transforms. The abstraction of using multi-dimensional arrays is a useful one to keep track of the offsets and strides involved, see +@fig:cooley-tukey-array.

We define the `Array` module to handle array manipulation in abstraction, similar to array slicing in NumPy.

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

### Numeric types

The `NumericType` type-class is defined so that we can constrain functions to work with `Float`, `Double` or `Complex a`. This also gives information on the byte size of each numeric type. We need to give `byteSize` an argument for it to be able to deduce the type, for which we use the `Proxy` type.

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

### Array structure

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
contiguous Array{shape,stride} = stride == fromShape shape (head stride)
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

Reshaping is only possible from an array that can be flattened to a one-dimensional array with constant stride. Otherwise the arithmetic of stepping through the resulting array would no longer be expressible in terms of a stride and offset.

``` {.haskell #array-methods}
reshape :: MonadError Text m => Shape -> Array a -> m (Array a)
reshape newShape array
    | (ndim array) == 1 = return $ array
        { shape = newShape
        , stride = fromShape newShape (head $ stride array) }
    | contiguous array = return $ array
        { shape = newShape
        , stride = fromShape newShape 1 }
    | otherwise = throwError "Cannot reshape multi-dimensional non-contiguous array."
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

# Codelets and Twiddles
## Codelets
A codelet, for the moment, is just some function that we can call.

``` {.haskell file=src/Codelet.hs}
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
```

## Calling GenFFT

This is an interface to the GenFFT executables. The most generic function here is `gen`, which can be passed a `Codelet` and a `GenFFTArgs` value, returning the generated code as `IO Text`.

```{.haskell file=src/GenFFT.hs}
{-# LANGUAGE RecordWildCards #-}

module GenFFT where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process
import Data.Maybe
import Codelet (Codelet(..), codeletName)

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

gen :: GenFFTArgs -> Codelet -> IO Text
gen args codelet@Codelet{..} = do
  let name' = codeletName codelet
  genFFT (tshow codeletType) $ ["-n", tshow codeletRadix] <> argList args{name=Just name'}
```

## Twiddle factors

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

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Array
import Lib

<<twiddle-factors-w>>
<<twiddle-factors-multi-w>>
<<twiddle-factors-make>>

factorsName :: Shape -> Text
factorsName s = "w_" <> T.intercalate "_" (map tshow s)
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
makeTwiddle shape = V.drop (head shape) $ V.fromList $ map (multiW shape) $ indices shape
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

To synthesise a larger FFT we have to duplicate some of the work done in GenFFT. We'll describe calling the codelets and the matching loop structure in terms of an *abstract syntax tree*. The AST will contain a subset of OpenCL focussed on computing FFTs.

Describing code structure is somewhat of a specialty of Haskell. Modern Haskell has features to describe syntax trees that are completely type safe, meaning that any expression where the types to not match up (for instance between function parameters, and applied arguments) is ill-formed to the point that they're impossible to construct. We need a few language extensions to make this work:

``` {.haskell #ast-haskell-extensions}
{-# LANGUAGE GADTs,DataKinds,TypeOperators,KindSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances,UndecidableInstances #-}
```

## Type lists

We need a way to declare the list of argument types to the codelets. In standard haskell this can be done by defining a recursive data-type, but the syntax would be ugly with lots of nested type expressions. The `DataKinds` and `TypeOperators` extensions let us define a type-level list structure and express it using normal list syntax (sometimes prefixed with a quote to disambiguate with data-level lists).

``` {.haskell #ast-typelists}
data HList :: [*] -> * where
    Nil :: HList '[]
    Cons :: a -> HList l -> HList (a ': l)

instance Show (HList '[]) where
    show Nil = "Nil"

instance (Show a, Show (HList l)) => Show (HList (a ': l)) where
    show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"
```

## OpenCL namespaces

OpenCL has several namespaces for different kinds of memory: `__constant`, `__global` and `__static` (there may be more, I don't know).

``` {.haskell #ast-ocl-namespaces}
data NameSpace = Static | Global | Constant
```

## Basic types

Many of the types that we define here exist just as type-level tags to larger expressions

``` {.haskell #ast-basic-types}
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
```

## Expressions

An expression is any unit of code that can be said to have some value. Expressions can be arguments to functions, or they can assigned to a variable.


``` {.haskell #ast-expression}
data Expr a where
    Literal        :: (Show a) => a -> Expr a
    IntegerValue   :: Int -> Expr Int
    RealValue      :: Double -> Expr Double
    ArrayRef       :: Array a -> Expr (Array a)
    VarReference   :: Variable a -> Expr a
    ArrayIndex     :: Array a -> [Expr Int] -> Expr a
    TUnit          :: Expr ()
    TNull          :: Expr (HList '[])
    (:+:)          :: (Show a) => Expr a -> Expr (HList b) -> Expr (HList (a ': b))
    Apply          :: Function a b -> Expr (HList a) -> Expr b

infixr 1 :+:
```

The most important entries here are `(:+:)` and `Apply`. The `:+:` operator is used to construct an argument list. This argument list can then be applied to a function using `Apply`. As an example, here is the definition of `planNoTwiddle`. This function builds the AST for calling a no-twiddle FFT codelet, given an input and output array.

``` {.haskell #synth-planNoTwiddle}
planNoTwiddle :: (MonadError Text m, RealFloat a) => NoTwiddleCodelet a -> Array (Complex a) -> Array (Complex a) -> m (Expr ())
planNoTwiddle f inp out = do
  is  <- stride inp !? 0
  os  <- stride out !? 0
  v   <- shape inp !? 1
  ivs <- stride inp !? 1
  ovs <- stride out !? 1
  return $ Apply f (ArrayRef (realPart inp) :+: ArrayRef (imagPart inp) :+:
                    ArrayRef (realPart out) :+: ArrayRef (imagPart out) :+:
                    Literal is :+: Literal os :+: Literal v :+: Literal ivs :+:
                    Literal ovs :+: TNull)
```

Note that the argument list is finalized with a `TNull`, otherwise the argument list would be improper. 

``` {.haskell #ast-statements}
data Range = Range
    { start :: Int
    , end   :: Int
    , step  :: Int } deriving (Show)


data Stmt where
    VarDeclaration   :: (Declarable a, Show a) => Variable a -> Stmt
    ConstDeclaration :: (Declarable a, Show a) => Constant a -> Stmt
    Expression       :: Expr () -> Stmt
    ParallelFor      :: Variable Int -> Range -> [Stmt] -> Stmt
    Assignment       :: (Show a) => Variable a -> Expr a -> Stmt
    FunctionDef      :: Function a b -> [Stmt] -> [Stmt] -> Stmt
```

## Generating code

Now we can generate C/OpenCL from our AST.

``` {.haskell #ast-syntax}
class Syntax a where
  generate :: a -> Text

instance Syntax (Expr a) where
  generate (Literal x) = tshow x
  generate (IntegerValue x) = tshow x
  generate (RealValue x) = tshow x
  generate (ArrayRef x)
    | (offset x) == 0 = name x
    | otherwise       = name x <> " + " <> tshow (offset x)
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

## AST Module

``` {.haskell file=src/AST.hs}
<<ast-haskell-extensions>>

module AST where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T

import Array
import Lib

<<ast-ocl-namespaces>>
<<ast-basic-types>>
<<ast-typelists>>
<<ast-expression>>
<<ast-statements>>

data FunctionDecl a b = FunctionDecl
  { functionName :: Text
  , argNames     :: [Text]
  , functionBody :: [Stmt] }

<<ast-syntax>>
```

# Synthesis of FFT

When discussing expressions in the abstract syntax tree, the definition of `planNoTwiddle` was shown. Similarly the twiddle codelet:

``` {.haskell #synth-planTwiddle}
planTwiddle :: (MonadError Text m, RealFloat a) => TwiddleCodelet a -> Array (Complex a) -> Array (Complex a) -> m (Expr ())
planTwiddle f inp twiddle = do
  rs  <- stride inp !? 0
  me  <- shape inp !? 1
  ms  <- stride inp !? 1
  return $ Apply f (ArrayRef (realPart inp) :+: ArrayRef (imagPart inp) :+:
                    ArrayRef (realPart twiddle) :+: Literal rs :+: Literal 0 :+: Literal me :+: Literal ms :+: TNull)
```

## Algorithm

An FFT algorithm now is a collection of codelets, twiddle factors and a list of statements defining how to synthesise the larger FFT.

``` {.haskell #synth-algorithm}
data Algorithm = Algorithm
  { codelets   :: Set Codelet
  , twiddles   :: Set Shape
  , statements :: [Stmt] }
```

To build up an algorithm from pieces, we derive an instance of `Monoid` for both `Algorithm` and `Either Text Algorithm`, the latter handling errors during code generation. We can use the `<>` operator to take the union of the sets of codelets and twiddles, as well as appending to the list of statements.

``` {.haskell #synth-algorithm}
instance Semigroup Algorithm where
  (Algorithm c1 t1 s1) <> (Algorithm c2 t2 s2) = Algorithm (c1 <> c2) (t1 <> t2) (s1 <> s2)

instance Monoid Algorithm where
  mempty = Algorithm mempty mempty mempty

instance {-# OVERLAPPING #-} Semigroup (Either Text Algorithm) where
  (Left a) <> _ = Left a
  _ <> (Left b) = Left b
  (Right a) <> (Right b) = Right (a <> b)

instance Monoid (Either Text Algorithm) where
  mempty = return mempty
```

## Synthesis Module

``` {.haskell file=src/Synthesis.hs}
{-# LANGUAGE DataKinds,GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Synthesis where

import Data.Complex (Complex(..))
import Data.Text (Text)
-- import qualified Data.Text as T

import Data.Set (Set)
import qualified Data.Set as S
import Data.List (sort)

import AST
import Array
import Lib
import Control.Monad.Except
import Codelet
import TwiddleFactors

import Math.NumberTheory.Primes.Factorisation (factorise)

(!?) :: MonadError Text m => [a] -> Int -> m a
[] !? i = throwError $ "List index error: list " <> tshow (i + 1) <> " too short."
(x:xs) !? i
  | i == 0    = return x
  | otherwise = xs !? (i - 1)

<<synth-planNoTwiddle>>
<<synth-planTwiddle>>
<<synth-algorithm>>

defineTwiddles :: Shape -> Algorithm
defineTwiddles shape = Algorithm mempty (S.fromList [shape]) mempty

noTwiddleFFT :: RealFloat a => Int -> Array (Complex a) -> Array (Complex a) -> Either Text Algorithm
noTwiddleFFT n inp out = do
  let codelet = Codelet NoTwiddle n
      f = Function (codeletName codelet) :: NoTwiddleCodelet a
  plan <- planNoTwiddle f inp out
  return $ Algorithm (S.fromList [codelet]) mempty [Expression plan]

twiddleFFT :: RealFloat a => Int -> Array (Complex a) -> Array (Complex a) -> Either Text Algorithm
twiddleFFT n inp twiddle = do
  let codelet = Codelet Twiddle n
      f = Function (codeletName codelet) :: TwiddleCodelet a
  plan <- planTwiddle f inp twiddle
  return $ Algorithm (S.fromList [codelet]) mempty [Expression plan]

nFactorFFT :: RealFloat a => [Int] -> Array (Complex a) -> Array (Complex a) -> Either Text Algorithm
nFactorFFT [] _ _         = return mempty
nFactorFFT [x] inp out    = noTwiddleFFT x inp out
nFactorFFT (x:xs) inp out = do
  let n = product xs
      w_array = Array (factorsName [n, x]) [n, x] (fromShape [n, x] 1) 0
      subfft i = do
          inp' <- reshape [n, x] =<< select 1 i inp
          out' <- reshape [x, n] =<< select 1 i out
          (nFactorFFT xs (transpose inp') out')
            <> (twiddleFFT x (transpose out') w_array)
            <> Right (defineTwiddles [n, x])

  l <- shape inp !? 1
  mconcat (map subfft [0..(l-1)])

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

