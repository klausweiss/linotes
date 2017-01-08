module Unit where

import Test.HUnit
import Database.Persist.Types (entityVal)

import DatabaseController
import Models.Note (noteContent)
import Widgets

main :: IO Counts
main = runTestTT $ TestList 
        [ testAddNote
        , testAddEmptyNote
        ]


testAddEmptyNote :: Test
testAddEmptyNote = TestCase (do
    db_file <- openDb "test.sqlite3"
    notes_count <- length <$> readNotes db_file
    saveNote db_file "" -- add empty note
    notes_count_new <- length <$> readNotes db_file
    assertEqual "Number of notes after adding an empty one" notes_count_new notes_count
    )

testAddNote :: Test
testAddNote = TestCase (do
    db_file <- openDb "test.sqlite3"
    notes_count <- length <$> readNotes db_file
    new_content <- return "Non-empty note"
    saveNote db_file new_content -- add note to database
    notes <- return $ readNotes db_file
    notes_count_new <- length <$> notes
    assertEqual "Number of notes after adding a new one" (notes_count + 1) notes_count_new 
    new_note_content <- noteContent . entityVal . head <$> notes
    assertEqual "Newest note content should match what was planned to be added" new_content new_note_content
    )
