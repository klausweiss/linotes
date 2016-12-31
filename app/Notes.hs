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
    )
import Brick.Util (on)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
    ( (<+>)
    , vBox
    , hLimit
    , vLimit
    , str
    , padAll
    )
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Graphics.Vty as Vty
import Lens.Micro ((^.))

import DatabaseController (readNotes)

main :: IO ()
main = do
    notes <- readNotes
    void $ M.defaultMain app (initialState notes)

app :: M.App (L.List () String) e ()
app = M.App { M.appDraw = drawUI
            , M.appStartEvent = return
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appAttrMap = const attrMap
            }

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI _list = [ui] where
    ui = vBox [ notes
              , noteContent]
    notes = B.borderWithLabel (str "Notes") $
        L.renderList listElemRenderer True _list
    noteContent = B.borderWithLabel (str "Content") $
            noteContainer "Tresc notatki"

noteContainer :: String -> Widget n
noteContainer content = 
    T.Widget Greedy Greedy $ do
        ctx <- getContext
        maxW <- return $ ctx ^. availWidthL
        render $ 
            hLimit maxW $
            str $ content ++ (take maxW $ repeat ' ')

listElemRenderer :: (Show a) => Bool -> a -> Widget ()
listElemRenderer selected elem = str $ show elem

appEvent :: L.List () String -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () String))
appEvent _list (T.VtyEvent e) =
    case e of
        Vty.EvKey Vty.KEsc [] -> M.halt _list
        ev -> M.continue =<< L.handleListEvent ev _list

attrMap :: AM.AttrMap
attrMap = AM.attrMap Vty.defAttr
    [ (L.listAttr,         Vty.white `on` Vty.blue)
    , (L.listSelectedAttr, Vty.red `on` Vty.white)
    ]

initialState :: [String] -> L.List () String
initialState _list = L.list () (Vec.fromList _list) 1
