module Widgets.Internal (fitInWidth) where

import Data.List.Split (chunksOf)
import Input (trim)


fitInWidth :: String -> Int -> String
fitInWidth text w | w <= 0 = text
fitInWidth text w = trim . foldl1 (\
        acc x -> acc ++ "\n" ++ x
    ) $ chunksOf w $ text ++ " "
