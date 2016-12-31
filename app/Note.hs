import DatabaseController (saveNote)

main :: IO ()
main = getLine >>= saveNote >> return ()
