module Input.Internal
    ( trim
    ) where

import Data.List (dropWhileEnd)
import Data.Char (isSpace)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
