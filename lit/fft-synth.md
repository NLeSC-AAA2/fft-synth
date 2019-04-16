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

<<array-list-manipulation>>
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

data ArrayIndex a = ArrayIndex
    { name     :: Text
    , index    :: Int } deriving (Show)

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
product :: Shape -> Int
product xs = foldr (*) 1 xs

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
reshape :: Array a -> Shape -> Either Text (Array a)
reshape array@Array{shape,stride} newShape
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
select :: Array a -> Int -> Int -> Either Text (Array a)
select array@Array{shape,stride,offset} dim i = do
    rcheck "dim" (ndim array) dim
    rcheck "size" (shape !! dim) i
    return $ array
        { shape  = remove shape dim
        , stride = remove stride dim
        , offset = offset + (stride !! dim) * i }

extrude :: Array a -> Int -> Either Text (Array a)
extrude array@Array{shape,stride} dim = do
    rcheck "dim" (ndim array + 1) dim
    return $ array
        { shape  = insert shape dim 1
        , stride = insert stride dim ((stride !! dim) * (shape !! dim)) }

slice :: Array a -> Int -> Int -> Int -> Int -> Either Text (Array a)
slice array@Array{shape,stride,offset} dim a b step = do
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

# Abstract Syntax Tree

``` {.haskell file=src/AST.hs}
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
```

# Miscellaneous functions

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

import Array
import Lib

main :: IO ()
main = hspec $ do
    describe "Strides.fromShape" $ do
        it "computes strides from shapes" $ do
            fromShape [3, 3, 3] 1 `shouldBe` [1, 3, 9]
            fromShape [2, 3, 5] 1 `shouldBe` [1, 2, 6]

    describe "Strides.remove" $ do
        it "drops indexed entry from list" $ do
            remove [1, 2, 3, 4] 0 `shouldBe` [2, 3, 4]
            remove [1, 2, 3, 4] 2 `shouldBe` [1, 2, 4]

    describe "Strides.replace" $ do
        it "replaces entry at index" $ do
            replace [1, 2, 3, 4] 0 7 `shouldBe` [7, 2, 3, 4]
            replace [1, 2, 3, 4] 2 7 `shouldBe` [1, 2, 7, 4]

    describe "Strides.insert" $ do
        it "inserts entry at index" $ do
            insert [1, 2, 3, 4] 0 7 `shouldBe` [7, 1, 2, 3, 4]
            insert [1, 2, 3, 4] 2 7 `shouldBe` [1, 2, 7, 3, 4]

    let a1 = floatArray "test" [4, 5]
    describe "Strides.select" $ do
        it "selects sub-array" $ do
            let a103 = select a1 0 3
            let a112 = select a1 1 2
            (shape <$> a103) `shouldBe` Right [5]
            (stride <$> a103) `shouldBe` Right [4]
            (offset <$> a103) `shouldBe` Right 3
            (shape <$> a112) `shouldBe` Right [4]
            (stride <$> a112) `shouldBe` Right [1]
            (offset <$> a112) `shouldBe` Right 8
```

