module Input.Internal
    ( trim
    ) where

import Data.List (dropWhileEnd)
import Data.Char (isSpace)

-- |The 'trim' function strips the string from leading spaces and extra spaces at the end
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
