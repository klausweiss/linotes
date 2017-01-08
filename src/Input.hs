module Input
    ( getNote
    ) where

import Input.Internal (trim)

getNote :: IO String
getNote = _getLine "" where
    _getLine acc = do
        line <- getLine
        _note <- return $ acc ++ "\n" ++ line
        if length line > 0
        then _getLine _note
        else return . trim $ _note
