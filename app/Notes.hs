{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

import qualified Brick.AttrMap as AM
import qualified Brick.Main as M
import Brick.Types
    ( Widget
    , Size(..)
    , getContext
    , availWidthL
    , render
    , ViewportType(..)
    )
import Brick.Util (on)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
    ( (<+>)
    , vBox
    , hLimit
    , vLimit
    , str
    , txt
    , viewport
    , withAttr
    , padAll
    )
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Graphics.Vty as Vty
import Lens.Micro ((^.), (&), (.~))
import Lens.Micro.TH (makeLenses)

import DatabaseController (readNotes)

-- to make Note content Viewport scrollable
data Name = VPNotes | VPNote
    deriving (Eq, Ord, Show)


-- application state
data St = St { _stNotes :: L.List Name String
             , _stSelected :: Maybe String
             }
makeLenses ''St -- to be able to extract list state from state for handleListEvent


main :: IO ()
main = do
    notes <- readNotes
    void $ M.defaultMain app (initialState notes)

app :: M.App St e Name
app = M.App { M.appDraw = drawUI
            , M.appStartEvent = return
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appAttrMap = const attrMap
            }

drawUI :: St -> [Widget Name]
drawUI st = [ui] where
    ui = vBox [ notes
              , noteContent]
    notes = B.borderWithLabel (str " Notes ") $
        L.renderList listElemRenderer True _list
    noteContent = padAll 1 $
            noteContainer $ case selected of 
                Nothing -> "--- [ note content ] ---"
                Just note -> noteContents note
    _list = st & _stNotes
    selected = st & _stSelected

noteContents :: String -> String
noteContents selected = selected

noteContainer :: String -> Widget Name
noteContainer content = 
    withAttr "noteContent" $
    T.Widget Greedy Greedy $ do
        ctx <- getContext
        maxW <- return $ ctx ^. availWidthL
        render $ 
            hLimit maxW $
            viewport VPNote Vertical $
            str $ content ++ (take maxW $ repeat ' ')

listElemRenderer :: Bool -> String -> Widget Name
listElemRenderer selected elem = str $ elem

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent e) = 
    let _list = st & _stNotes 
    in case e of
          Vty.EvKey Vty.KEnter [] -> 
              let elem = L.listSelectedElement _list
              in case elem of
                Nothing -> M.continue st
                Just (i, note) -> M.continue $ st & stSelected .~ Just note

          Vty.EvKey Vty.KEsc [] -> M.halt st

          ev -> M.continue =<< T.handleEventLensed st stNotes L.handleListEvent ev

attrMap :: AM.AttrMap
attrMap = AM.attrMap Vty.defAttr
    [ (L.listAttr,         Vty.white `on` Vty.black)
    , (L.listSelectedAttr, Vty.red `on` Vty.white)
    , ("noteContent",      Vty.yellow `on` Vty.black)
    ]

initialState :: [String] -> St
initialState _list =
    St { _stNotes = L.list VPNotes (Vec.fromList _list) 1
       , _stSelected = Nothing
       }
