import DatabaseController
    ( saveNote
    , dbFile
    )
import Input (getNote)

main :: IO ()
main = getNote >>= \note ->
       dbFile >>= \db_file ->
       saveNote db_file note >>
       return ()
