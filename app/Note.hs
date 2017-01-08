{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)

import DatabaseController
    ( saveNote
    , openDefaultDb
    )
import Input (getNote)

main :: IO ()
main = getArgs >>= \case
        [] -> getNote 
        text -> return (unwords text)
       >>= \note ->
       openDefaultDb >>= \db_file ->
       saveNote db_file note >>
       return ()
