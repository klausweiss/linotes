module Widgets.Internal (fitInWidth) where

import Data.List.Split (chunksOf)
import Input (trim)


-- |The 'fitInWidth' function returns a string with lines broken using '\n' so they are not longer than specified number of charactrers.
fitInWidth :: String    -- ^ Input string
           -> Int       -- ^ Maximum number of characters in one line
           -> String    -- ^ The string with broken lines
fitInWidth text w | w <= 0 = text
fitInWidth text w = trim . foldl1 (\
        acc x -> acc ++ "\n" ++ x
    ) $ chunksOf w $ text ++ " "
