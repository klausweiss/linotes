module Widgets
    ( noteContainer        
    ) where


import qualified Brick.Types as T
import Brick.Widgets.Core
    ( hLimit
    , str
    , viewport
    , withAttr
    )
import Data.List.Split
import Lens.Micro ((^.), (&))


noteContainer :: (Ord a, Show a) => a -> String -> T.Widget a
noteContainer _vp content = 
    T.Widget T.Greedy T.Greedy $ do
        ctx <- T.getContext
        maxW <- return $ ctx ^. T.availWidthL
        T.render $ 
            hLimit maxW $
            viewport _vp T.Vertical $
            str $ fitInWidth content maxW -- ++ (take maxW $ repeat ' ')

fitInWidth :: String -> Int -> String
fitInWidth text w = foldl1 (\
        acc x -> acc ++ "\n" ++ x
    ) $ chunksOf w $ text ++ " "

