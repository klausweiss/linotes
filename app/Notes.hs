{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import qualified Brick.AttrMap as AttrMap
import qualified Brick.Main as Main
import Brick.Types
    ( Widget
    )
import Brick.Util (on)
import qualified Brick.Widgets.Border as Border
import Brick.Widgets.Core
    ( (<+>)
    , vBox
    , hLimit
    , vLimit
    , str
    )
import qualified Brick.Types as Types
import qualified Brick.Widgets.List as List
import qualified Data.Vector as Vec
import qualified Graphics.Vty as Vty

import DatabaseController (readNotes)

main :: IO ()
main = do
    notes <- readNotes
    void $ Main.defaultMain app (initialState notes)

app :: Main.App (List.List () String) e ()
app = Main.App { Main.appDraw = drawUI
               , Main.appStartEvent = return
               , Main.appChooseCursor = Main.showFirstCursor
               , Main.appHandleEvent = appEvent
               , Main.appAttrMap = const attrMap
               }

drawUI :: (Show a) => List.List () a -> [Widget ()]
drawUI _list = [ui] where
    ui = vBox [box]
    box = Border.borderWithLabel (str "Notes") $
        hLimit 25 $
        vLimit 5 $
        List.renderList listElemRenderer True _list

listElemRenderer :: (Show a) => Bool -> a -> Widget ()
listElemRenderer selected elem = str $ show elem

appEvent :: List.List () String -> Types.BrickEvent () e -> Types.EventM () (Types.Next (List.List () String))
appEvent _list (Types.VtyEvent e) =
    case e of
        Vty.EvKey Vty.KEsc [] -> Main.halt _list
        ev -> Main.continue =<< List.handleListEvent ev _list

attrMap :: AttrMap.AttrMap
attrMap = AttrMap.attrMap Vty.defAttr
    [ (List.listAttr,         Vty.white `on` Vty.blue)
    , (List.listSelectedAttr, Vty.red `on` Vty.white)
    ]

initialState :: [String] -> List.List () String
initialState _list = List.list () (Vec.fromList _list) 1
