{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Miso
import Miso.String (MisoString, ms)
import qualified Miso.String as M

import Data.Bool (bool)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Maybe (fromJust)

type Model = IntMap MisoString

initialModel :: Model
initialModel = Map.singleton 1 "Enjoy life :)"

data Action
  = NoOp
  | Add (Map.Key, MisoString)
  | New

main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model         = initialModel
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    subs          = []
    mountPoint    = Nothing

updateModel :: Action -> Model -> Effect Action Model
updateModel New m = noEff $ Map.insert (nextKey m) "" m
updateModel (Add kv) m = noEff $ (uncurry Map.insert kv) m
updateModel NoOp m = noEff m

nextKey :: IntMap a -> Int
nextKey = fst . fst . fromJust . Map.maxViewWithKey

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "contentWrapper" ]
  [ h1_ []
      [ text "Just "
      , em_ [] [ text "To Do" ]
      , text " It, Basically"
      ]
  , article_ [ class_ "post" ]
      [ h2_ [] [ text "Enter and edit your list at will" ]
      , p_ [] [ text lorem ]
      ]
  , article_ []
      [ ul_ [ id_ "theList", boolProp "contenteditable" True ] $
          Map.foldrWithKey' (\k item xs -> entry k item : xs) [] model
      ]
  ]

lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. \
       \ Praesent non  mauris orci, quis tincidunt sapien. Vestibulum \
       \ sodales est ut diam  tincidunt non bibendum dui cursus. Nulla \
       \ blandit iaculis ipsum nec  ullamcorper. In mattis gravida magna \
       \ et elementum."

entry :: Map.Key -> MisoString -> View Action
entry k item = li_
  [ onInput (Add . (k,))
  , onKeyDown (bool NoOp New . (== KeyCode 13))
  ]
  [ text item ]
