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
    ( openDefaultDb
    , openDb
    , deleteNote
    , readNotes
    , saveNote
    ) where

import Database.Persist.Sqlite
    ( delete
    , Entity
    , entityKey
    , insert
    , runSqlite
    , runMigration
    , selectList
    , SelectOpt(..)
    )
import Data.Time (getCurrentTime)
import Data.Text (pack)
import Control.Monad.IO.Class
    ( liftIO
    , MonadIO
    )
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Directory
    ( getHomeDirectory
    , createDirectoryIfMissing)

import Models.Note


openDefaultDb :: IO FilePath
openDefaultDb = openDb "notes.sqlite3"

openDb :: String -> IO FilePath
openDb filename = do
    home <- getHomeDirectory
    notes_parent_name <- return "/.linotes/"
    db_file_path <- return filename
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

deleteNote :: (Monad m, MonadIO m, MonadBaseControl IO m) => String -> Entity Note -> m ()
deleteNote db_file note = runSqlite (pack db_file) $ do
    runMigration migrateAll
    delete (entityKey note)
