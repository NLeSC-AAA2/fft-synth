-- ------ language="Haskell" file="src/Lib.hs"
module Lib where

import Data.Text (Text)
import qualified Data.Text as T

-- ------ begin <<lib-list-manipulation>>[0]
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
-- ------ end

tshow :: Show a => a -> Text
tshow = T.pack . show
-- ------ end
