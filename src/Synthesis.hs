-- ------ language="Haskell" file="src/Synthesis.hs"
{-# LANGUAGE DataKinds,GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Synthesis where

-- import Debug.Trace

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

type NoTwiddleCodelet a = Function
  [ Array a, Array a
  , Array a, Array a
  , Int, Int, Int, Int, Int ] ()

type TwiddleCodelet a = Function
  [ Array a, Array a
  , Array a
  , Int, Int, Int, Int ] ()

(!?) :: MonadError Text m => [a] -> Int -> m a
[] !? i = throwError $ "List index error: list " <> tshow (i + 1) <> " too short."
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

data Algorithm = Algorithm
  { codelets   :: Set Codelet
  , twiddles   :: Set Shape
  , statements :: [Stmt] }

defineTwiddles :: Shape -> Algorithm
defineTwiddles shape = Algorithm mempty (S.fromList [shape]) mempty

-- instance (Monad m) => Semigroup (m Algorithm) where
--   a <> b = do
--     a' <- a
--     b' <- b
--     return (a' <> b')

instance Semigroup Algorithm where
  (Algorithm c1 t1 s1) <> (Algorithm c2 t2 s2) = Algorithm (c1 <> c2) (t1 <> t2) (s1 <> s2)

instance Monoid Algorithm where
  mempty = Algorithm mempty mempty mempty

instance {-# OVERLAPPING #-} Semigroup (Either Text Algorithm) where
  (Left a) <> _ = Left a
  _ <> (Left b) = Left b
  (Right a) <> (Right b) = Right (a <> b)

instance (Monad m, Semigroup (m Algorithm)) => Monoid (m Algorithm) where
  mempty = return mempty

noTwiddleFFT :: (MonadError Text m, RealFloat a) => Int -> Array (Complex a) -> Array (Complex a) -> m Algorithm
noTwiddleFFT n inp out = do
  let codelet = Codelet NoTwiddle n
      f = Function (codeletName codelet) :: NoTwiddleCodelet a
  plan <- planNoTwiddle f inp out
  return $ Algorithm (S.fromList [codelet]) mempty [Expression plan]

twiddleFFT :: (MonadError Text m, RealFloat a) => Int -> Array (Complex a) -> Array (Complex a) -> m Algorithm
twiddleFFT n inp twiddle = do
  let codelet = Codelet Twiddle n
      f = Function (codeletName codelet) :: TwiddleCodelet a
  plan <- planTwiddle f inp twiddle
  return $ Algorithm (S.fromList [codelet]) mempty [Expression plan]

-- nFactorFFT :: (MonadError Text m, RealFloat a) => [Int] -> Array (Complex a) -> Array (Complex a) -> m Algorithm
nFactorFFT :: RealFloat a => [Int] -> Array (Complex a) -> Array (Complex a) -> Either Text Algorithm
nFactorFFT [] _ _         = return mempty
nFactorFFT [x] inp out    = noTwiddleFFT x inp out
nFactorFFT (x:xs) inp out = do
  -- | trace ("nFactorFFT " ++ show (x:xs) ++ " " ++ show inp ++ " " ++ show out) True = do
  let n = product xs
      w_array = Array (factorsName [n, x]) [n, x] (fromShape [n, x] 1) 0
      subfft i = do
          -- | trace ("subfft " ++ show i) True = do 
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
-- ------ end
