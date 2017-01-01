import DatabaseController (saveNote, dbFile)

main :: IO ()
main = getLine >>= \note ->
       dbFile >>= \db_file ->
       saveNote db_file note >>
       return ()
