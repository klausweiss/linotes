{-# LANGUAGE OverloadedStrings #-}
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
import Lens.Micro ((^.))

import DatabaseController (readNotes)

data Name = VPNotes | VPNote
    deriving (Eq, Ord, Show)

main :: IO ()
main = do
    notes <- readNotes
    void $ M.defaultMain app (initialState notes)

app :: M.App (L.List Name String) e Name
app = M.App { M.appDraw = drawUI
            , M.appStartEvent = return
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appAttrMap = const attrMap
            }

drawUI :: (Show a) => L.List Name a -> [Widget Name]
drawUI _list = [ui] where
    ui = vBox [ notes
              , noteContent]
    notes = B.borderWithLabel (str " Notes ") $
        L.renderList listElemRenderer True _list
    noteContent = padAll 1 $
            noteContainer "Tresc no\nelaosdfkasdfak sjhdfkajsdhf laksjdhflkasjdhf laksjdhf aksljdhf alksjdfhaklsdjhfalksdjhfalskdj halsdkjfha lskdjfhalksdjhf alskjdfhalsk jhfalksdjhf alskdjhf alskdjfh alskdjfftatki"

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

listElemRenderer :: (Show a) => Bool -> a -> Widget Name
listElemRenderer selected elem = str $ show elem

appEvent :: L.List Name String -> T.BrickEvent Name e -> T.EventM Name (T.Next (L.List Name String))
appEvent _list (T.VtyEvent e) =
    case e of
        Vty.EvKey Vty.KEnter [] -> M.halt _list
        Vty.EvKey Vty.KEsc [] -> M.halt _list
        ev -> M.continue =<< L.handleListEvent ev _list

attrMap :: AM.AttrMap
attrMap = AM.attrMap Vty.defAttr
    [ (L.listAttr,         Vty.white `on` Vty.black)
    , (L.listSelectedAttr, Vty.red `on` Vty.white)
    , ("noteContent",      Vty.yellow `on` Vty.black)
    ]

initialState :: [String] -> L.List Name String
initialState _list = L.list VPNotes (Vec.fromList _list) 1
