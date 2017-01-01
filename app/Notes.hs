{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (void)
import Data.List (lines)

import qualified Brick.AttrMap as AM
import qualified Brick.Main as M
import Brick.Util (on)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
    ( vBox
    , str
    , withAttr
    )
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import Database.Persist.Types
    ( Entity
    , entityVal)
import qualified Graphics.Vty as Vty
import Lens.Micro ((&), (.~))
import Lens.Micro.TH (makeLenses)

import DatabaseController
    ( readNotes
    , deleteNote
    , dbFile
    )
import Widgets
import Models.Note
    ( noteContent
    , noteLastModified
    , Note
    )

-- to make Note content Viewport scrollable
data Name = VPNotes | VPNote
    deriving (Eq, Ord, Show)


-- application state
data St = St { _stNotes :: L.List Name (Entity Note)
             , _stSelected :: Maybe (Entity Note)
             }
makeLenses ''St -- to be able to extract list state from state for handleListEvent


main :: IO ()
main = do
    db_file <- dbFile
    notes <- readNotes db_file
    void $ M.defaultMain app (initialState notes)

app :: M.App St e Name
app = M.App { M.appDraw = drawUI
            , M.appStartEvent = return
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appAttrMap = const attrMap
            }

drawUI :: St -> [T.Widget Name]
drawUI st = [ui] where
    ui = vBox [ notes
              , noteContentView ]
    notes = B.borderWithLabel (str " Notes ") $
        L.renderList listElemRenderer True _list
    noteContentView = B.border $
            withAttr "noteContent" $
            noteContainer VPNote $ case selected of 
                Nothing -> "--- [ note content ] ---"
                Just note -> noteContent . entityVal $ note
    _list = st & _stNotes
    selected = st & _stSelected

listElemRenderer :: Bool -> Entity Note -> T.Widget Name
listElemRenderer selected elem = str . (\
        note -> (take 19 . show . noteLastModified $ note) ++ " " ++ (head . lines . noteContent $ note)
    ) $ entityVal elem

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent e) = 
    let _list = st & _stNotes 
    in case e of
          Vty.EvKey Vty.KEnter [] -> openNoteEv st _list

          Vty.EvKey Vty.KDel [] -> deleteNoteEv st _list
          Vty.EvKey (Vty.KChar 'd') [] -> deleteNoteEv st _list

          Vty.EvKey Vty.KEsc [] -> M.halt st
          Vty.EvKey (Vty.KChar 'q') [] -> M.halt st

          ev -> M.continue =<< T.handleEventLensed st stNotes L.handleListEvent ev

attrMap :: AM.AttrMap
attrMap = AM.attrMap Vty.defAttr
    [ (L.listAttr,          Vty.white `on` Vty.black)
    , (L.listSelectedAttr,  Vty.red `on` Vty.white)
    , ("noteContent",       Vty.yellow `on` Vty.black)
    ]

initialState :: [Entity Note] -> St
initialState _list =
    St { _stNotes = L.list VPNotes (Vec.fromList _list) 1
       , _stSelected = Nothing
       }


-- EVENTS

openNoteEv :: St -> L.List Name (Entity Note) -> T.EventM Name (T.Next St)
openNoteEv st _list =
    let elem = L.listSelectedElement _list
    in case elem of
        Nothing -> M.continue st
        Just (i, note) -> M.continue $ st & stSelected .~ Just note

deleteNoteEv :: St -> L.List Name (Entity Note) -> T.EventM Name (T.Next St)
deleteNoteEv st _list =
    let elem = L.listSelectedElement _list
    in case elem of
        Nothing -> M.continue st
        Just(i, note) -> M.suspendAndResume $ do
            db_file <- dbFile
            deleteNote db_file note
            return $ st & stNotes .~ L.listRemove i _list
