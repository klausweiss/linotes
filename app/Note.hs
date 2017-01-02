import System.Environment (getArgs)

import DatabaseController
    ( saveNote
    , dbFile
    )
import Input (getNote)

main :: IO ()
main = getArgs >>= \args ->
       case args of
        [] -> getNote 
        text -> return (unwords text)
       >>= \note ->
       dbFile >>= \db_file ->
       saveNote db_file note >>
       return ()
