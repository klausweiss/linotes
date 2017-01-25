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


-- |Returns a handle to the default database file used by the application
openDefaultDb :: IO FilePath
openDefaultDb = openDb "notes.sqlite3"

-- |Returns a handle to database file
openDb :: String        -- ^ Filename (placed in ~\/.linotes\/) - will be created if necessary
       -> IO FilePath   -- ^ Handle to database file wrapped in an IO monad
openDb filename = do
    home <- getHomeDirectory
    let notes_parent_name = "/.linotes/"
    let db_file_path = filename
    let notes_parent = home ++ notes_parent_name
    createDirectoryIfMissing True notes_parent
    let db_file = notes_parent ++ db_file_path
    return db_file

-- |Saves a note in given SQLite database.
saveNote :: String      -- ^ SQLite database file path
         -> String      -- ^ Note content
         -> IO String   -- ^ Note content wrapped in an IO monad
saveNote db_file note = runSqlite (pack db_file) $ do
    runMigration migrateAll
    if length note > 0 then do
        time <- liftIO getCurrentTime
        noteId <- insert $ Note note time
        return note
    else return ""

-- |Returns a list of note entities from given SQLite database.
readNotes :: String             -- ^ SQLite database file path
          -> IO [Entity Note]   -- ^ List of note entities wrapped in an IO monad
readNotes db_file = runSqlite (pack db_file) $ do
    runMigration migrateAll
    notes <- selectList [] [Desc NoteLastModified]
    return $ notes

-- |Deletes a note entity from given SQLite database.
deleteNote :: (Monad m, MonadIO m, MonadBaseControl IO m)
           => String        -- ^ SQLite database file path
           -> Entity Note   -- ^ The entity to be deleted
           -> m ()
deleteNote db_file note = runSqlite (pack db_file) $ do
    runMigration migrateAll
    delete (entityKey note)
