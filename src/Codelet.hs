-- ------ language="Haskell" file="src/Codelet.hs"
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
-- ------ end
