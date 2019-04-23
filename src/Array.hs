-- ------ language="Haskell" file="src/Array.hs"
{-# LANGUAGE DuplicateRecordFields #-}

module Array where

import Data.Complex (Complex(..))
import Data.Proxy

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Except

import Lib


-- ------ begin <<array-numeric-class>>[0]
class NumericType a where
    byteSize :: proxy a -> Int

instance NumericType Float where
    byteSize _ = 4

instance NumericType Double where
    byteSize _ = 8

instance (NumericType a) => NumericType (Complex a) where
    byteSize _ = 2 * (byteSize (Proxy :: Proxy a))
-- ------ end
-- ------ begin <<array-types>>[0]
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
-- ------ end
-- ------ begin <<array-methods>>[0]
fromShape :: Shape -> Int -> Stride
fromShape [] _ = []
fromShape (x:xs) n = n : fromShape xs (n * x)
-- ------ end
-- ------ begin <<array-methods>>[1]
ndim :: Array a -> Int
ndim = length . shape

contiguous :: Array a -> Bool
contiguous Array{shape,stride} = stride == fromShape shape 1
-- ------ end
-- ------ begin <<array-methods>>[2]
rcheck :: MonadError Text m => Text -> Int -> Int -> m ()
rcheck what n i
    | (i >= 0) && (i < n) = return ()
    | otherwise = throwError $ "Range check error: " <> what <> " "
                      <> tshow n <> " " <> tshow i
-- ------ end
-- ------ begin <<array-methods>>[3]
realPart :: Array (Complex a) -> Array a
realPart array@Array{stride} = array
    { stride = map (* 2) stride }

imagPart :: Array (Complex a) -> Array a
imagPart array@Array{stride, offset} = array
    { stride = map (* 2) stride
    , offset = offset + 1 }
-- ------ end
-- ------ begin <<array-methods>>[4]
transpose :: Array a -> Array a
transpose array@Array{shape, stride} = array
    { shape = reverse shape
    , stride = reverse stride }
-- ------ end
-- ------ begin <<array-methods>>[5]
reshape :: Array a -> Shape -> Either Text (Array a)
reshape array@Array{shape,stride} newShape
    | contiguous array = return $ array
        { shape = newShape
        , stride = fromShape newShape 1 }
    | otherwise = throwError "Cannot reshape non-contiguous array."
-- ------ end
-- ------ begin <<array-methods>>[6]
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
-- ------ end
-- ------ end
