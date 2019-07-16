-- ------ language="Haskell" file="src/Synthesis.hs"
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

-- ------ begin <<synth-planNoTwiddle>>[0]
planNoTwiddle
    :: (MonadError Text m, RealFloat a)
    => NoTwiddleCodelet a -> Array (Complex a) -> Array (Complex a) -> m (Expr ())
planNoTwiddle f inp out = do
  is  <- stride inp !? 0
  os  <- stride out !? 0
  v   <- shape inp !? 1
  ivs <- stride inp !? 1
  ovs <- stride out !? 1
  return $ Apply f (ArrayRef (realPart inp) :+: ArrayRef (imagPart inp) :+:
                    ArrayRef (realPart out) :+: ArrayRef (imagPart out) :+:
                    Literal is :+: Literal os :+: Literal v :+: Literal ivs :+:
                    Literal ovs :+: Empty)
-- ------ end
-- ------ begin <<synth-planTwiddle>>[0]
planTwiddle
    :: (MonadError Text m, RealFloat a)
    => TwiddleCodelet a -> Array (Complex a) -> Array (Complex a) -> m (Expr ())
planTwiddle f inp twiddle = do
  rs  <- stride inp !? 0
  me  <- shape inp !? 1
  ms  <- stride inp !? 1
  return $ Apply f (ArrayRef (realPart inp) :+: ArrayRef (imagPart inp) :+:
                    ArrayRef (realPart twiddle) :+: Literal rs :+: Literal 0 :+:
                    Literal me :+: Literal ms :+: Empty)
-- ------ end
-- ------ begin <<synth-algorithm>>[0]
data Algorithm = Algorithm
  { codelets   :: Set Codelet
  , twiddles   :: Set Shape
  , statements :: [Stmt] }
-- ------ end
-- ------ begin <<synth-algorithm>>[1]
instance Semigroup Algorithm where
  (Algorithm c1 t1 s1) <> (Algorithm c2 t2 s2) = Algorithm (c1 <> c2) (t1 <> t2) (s1 <> s2)

instance Monoid Algorithm where
  mempty = Algorithm mempty mempty mempty

newtype HandleError a = Handle { unHandle :: Either Text a } deriving (Functor, Applicative, Monad, MonadError Text)

instance Semigroup a => Semigroup (HandleError a) where
  Handle (Left a) <> _ = Handle $ Left a
  _ <> Handle (Left b) = Handle $ Left b
  Handle (Right a) <> Handle (Right b) = Handle $ Right (a <> b)

instance Monoid a => Monoid (HandleError a) where
  mempty = Handle $ Right mempty
-- ------ end

defineTwiddles :: Shape -> Algorithm
defineTwiddles shape = Algorithm mempty (S.fromList [shape]) mempty

noTwiddleFFT
    :: RealFloat a
    => Int -> Array (Complex a) -> Array (Complex a) -> HandleError Algorithm
noTwiddleFFT n inp out = Handle $ do
  let codelet = Codelet NoTwiddle n
      f = Function (codeletName codelet) :: NoTwiddleCodelet a
  plan <- planNoTwiddle f inp out
  return $ Algorithm (S.fromList [codelet]) mempty [Expression plan]

twiddleFFT
    :: RealFloat a
    => Int -> Array (Complex a) -> Array (Complex a) -> HandleError Algorithm
twiddleFFT n inp twiddle = Handle $ do
  let codelet = Codelet Twiddle n
      f = Function (codeletName codelet) :: TwiddleCodelet a
  plan <- planTwiddle f inp twiddle
  return $ Algorithm (S.fromList [codelet]) mempty [Expression plan]

nFactorFFT
    :: RealFloat a
    => [Int] -> Array (Complex a) -> Array (Complex a) -> HandleError Algorithm
nFactorFFT [] _ _ = mempty
nFactorFFT [x] inp out = noTwiddleFFT x inp out
nFactorFFT (x:xs) inp out = do
    let n = product xs
        w_array = Array (factorsName [n, x]) [n, x] (fromShape [n, x] 1) 0
        subfft i = do
            inp' <- reshape [x, n] =<< select 1 i inp
            out' <- reshape [n, x] =<< select 1 i out
            nFactorFFT xs (transpose inp') out'
                <> twiddleFFT x (transpose out') w_array
                <> Handle (Right (defineTwiddles [n, x]))

    l <- shape inp !? 1
    foldMap subfft [0..(l-1)]

factors :: Int -> [Int]
factors n = sort $ concatMap (\(i, m) -> take m $ repeat (fromIntegral i :: Int)) (factorise $ fromIntegral n)

fullFactorFFT :: RealFloat a => Int -> Array (Complex a) -> Array (Complex a) -> Either Text Algorithm
fullFactorFFT n x y = unHandle $ nFactorFFT (factors n) x y
-- ------ end
