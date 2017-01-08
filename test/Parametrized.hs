module Parametrized where

import Data.List.Split (wordsBy)
import Test.QuickCheck

import Input (trim)
import Widgets.Internal (fitInWidth)

main :: IO ()
main = sequence_ $
    [ quickCheck prop_trim
    , quickCheck prop_fit_in_width
    ]


prop_trim :: String -> Bool
prop_trim note = (length tr_note) == 0 ||
                 (head tr_note) /= ' ' &&
                 (last tr_note) /= ' '
                    where tr_note = trim note

prop_fit_in_width :: String -> Int -> Bool
prop_fit_in_width text lim | lim > 0 =
    allTrue $
    -- all of the lines are shorter or equal lim
    [ (maximum $ 0:(map length . wordsBy (== '\n') $ fittedText)) <= lim
    -- number of lines >= length of text / limit
    , lim * ((+1) . length . filter (== '\n') $ fittedText) >= length text
    ]
        where allTrue = and
              fittedText = fitInWidth text lim
prop_fit_in_width text lim = text == fitInWidth text lim
