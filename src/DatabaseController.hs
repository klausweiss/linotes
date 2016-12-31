{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DatabaseController
    ( saveNote
    , readNotes
    ) where

import Database.Persist
import Database.Persist.Sqlite hiding (migrate)
import Database.Persist.TH
import Data.Time
import Control.Monad.IO.Class

import Models.Note

db_file = "notes.sqlite3"


saveNote :: String -> IO String
saveNote note = runSqlite db_file $ do
    runMigration migrateAll
    time <- liftIO getCurrentTime
    noteId <- insert $ Note note time
    return note

readNotes :: IO [Note]
readNotes = runSqlite db_file $ do
    runMigration migrateAll
    notes <- selectList [] [Desc NoteLastModified]
    return $ map entityVal notes
