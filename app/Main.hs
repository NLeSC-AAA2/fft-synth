{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Environment
import Data.Complex (Complex(..), realPart, imagPart)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Vector.Unboxed as V

import Lib
import AST
import Array (Array(..), Shape, fromShape)
import Synthesis
import GenFFT
import TwiddleFactors

main :: IO ()
main = do
  T.IO.putStrLn genMacros
  args <- getArgs
  case args of
    [] -> putStrLn "Missing argument!"
    (n:factors) -> printFFT (read n) (map read factors)

generateTwiddle :: Text -> Shape -> IO Text
generateTwiddle typeName shape = do
  let showComplex z = tshow (realPart z) <> ", " <> tshow (imagPart z)
      def = "__constant " <> typeName <> " " <> factorsName shape <> "["
            <> tshow (2 * (product shape - head shape)) <> "] = {\n"
            <> T.intercalate "," (map showComplex (V.toList $ makeTwiddle shape))
            <> "\n};"
  indent def

writeAlgorithm :: Algorithm -> IO Text
writeAlgorithm Algorithm{..} = do
  twdefs <- T.unlines <$> mapM (generateTwiddle "float") (S.toList twiddles)
  codes  <- T.unlines <$> mapM (gen defaultArgs) (S.toList codelets)
  return $ twdefs <> codes
        <> "void fft(float const *input, float *output) {\n"
        <> T.unlines (map (("    " <>) . generate) statements)
        <> "}"

printFFT :: Int -> [Int] -> IO ()
printFFT n factors = do
  case alg' of
    Left err -> T.IO.putStrLn $ "Error: " <> err
    Right alg -> T.IO.putStrLn =<< writeAlgorithm alg
  where
    shape = [n, 1 :: Int]
    a = Array "input" shape (fromShape shape 1) 0 :: Array (Complex Double)
    b = Array "output" shape (fromShape shape 1) 0 :: Array (Complex Double)
    --alg' = fullFactorFFT n a b
    --alg' = unHandle $ nFactorFFT [32, 32] a b
    alg' = case factors of
                [] -> fullFactorFFT n a b
                (_:_) -> unHandle $ nFactorFFT factors a b
