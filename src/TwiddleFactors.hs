-- ------ language="Haskell" file="src/TwiddleFactors.hs"
module TwiddleFactors where

import Data.Complex

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Array
import Lib

-- ------ begin <<twiddle-factors-w>>[0]
w :: RealFloat a => Int -> Int -> Complex a
w n k = cis (2 * pi * fromIntegral k / fromIntegral n)
-- ------ end
-- ------ begin <<twiddle-factors-multi-w>>[0]
multiW :: RealFloat a => Shape -> [Int] -> Complex a
multiW n k = w (product n) (product k)
-- ------ end
-- ------ begin <<twiddle-factors-make>>[0]
indices :: Shape -> [[Int]]
indices = foldr (\ n -> concatMap (\ idcs -> map (: idcs) [0 .. n-1])) [[]]

makeTwiddle :: Shape -> Vector (Complex Double)
makeTwiddle shape = V.fromList . map (multiW shape) . drop (head shape) $ indices shape
-- ------ end

factorsName :: Shape -> Text
factorsName s = "w_" <> T.intercalate "_" (map tshow s)
-- ------ end
