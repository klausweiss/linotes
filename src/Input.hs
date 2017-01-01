module Input
    ( getNote
    ) where

import Data.List (dropWhileEnd)
import Data.Char (isSpace)

getNote :: IO String
getNote = _getLine "" where
    _getLine acc = do
        line <- getLine
        _note <- return $ acc ++ "\n" ++ line
        if length line > 0
        then _getLine _note
        else return . trim $ _note

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
