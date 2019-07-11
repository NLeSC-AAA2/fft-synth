-- ------ language="Haskell" file="src/GenFFT.hs"
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
  , opencl = False
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
-- ------ end
