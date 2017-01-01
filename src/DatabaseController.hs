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
    ( dbFile
    , deleteNote
    , readNotes
    , saveNote
    ) where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Data.Text (pack)
import Control.Monad.IO.Class
import System.Directory

import Models.Note

dbFile :: IO FilePath
dbFile = do
    home <- getHomeDirectory
    notes_parent_name <- return "/.linotes/"
    db_file_path <- return "notes.sqlite3"
    notes_parent <- return $ home ++ notes_parent_name
    createDirectoryIfMissing True notes_parent
    db_file <- return $ notes_parent ++ db_file_path
    return db_file


saveNote :: String -> String -> IO String
saveNote db_file note = runSqlite (pack db_file) $ do
    runMigration migrateAll
    if length note > 0 then do
        time <- liftIO getCurrentTime
        noteId <- insert $ Note note time
        return note
    else return ""

readNotes :: String -> IO [Entity Note]
readNotes db_file = runSqlite (pack db_file) $ do
    runMigration migrateAll
    notes <- selectList [] [Desc NoteLastModified]
    return $ notes

deleteNote db_file note = runSqlite (pack db_file) $ do
    runMigration migrateAll
    delete (entityKey note)
