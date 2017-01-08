module Input
    ( getNote
    ) where

import Input.Internal (trim)

-- |The 'getNote' function gets multi-line input from user. 
-- It returns value when user typed two Enter characters in a row.
getNote :: IO String
getNote = _getLine "" where
    _getLine acc = do
        line <- getLine
        _note <- return $ acc ++ "\n" ++ line
        if length line > 0
        then _getLine _note
        else return . trim $ _note
