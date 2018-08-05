{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Miso
import Miso.String (MisoString, ms)
import qualified Miso.String as S

import Data.Bool (bool)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid

data Item = Item
  { itemDescription :: MisoString
  , editing         :: Bool
  } deriving (Eq)

data Model = Model
  { allEntries   :: IntMap Item
  , currentEntry :: Maybe (Map.Key, MisoString)
  } deriving (Eq)

initialModel :: Model
initialModel =
  Model (Map.singleton 1 $ Item "Enjoy life :)" False) Nothing

data Action
  = NoOp
  | Edit Map.Key
  | Input (Map.Key, MisoString)
  | Add
  | Delete

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
updateModel NoOp m = noEff m
updateModel (Edit k) m@Model{..} =
  m { allEntries = disablePrevEditing currentEntry $
                     Map.update (Just . enableEditing) k allEntries
    , currentEntry = Just (k , itemDescription $ allEntries Map.! k)
    } <# do
    focus $ "item-" <> S.ms k
    pure NoOp
updateModel (Input kv) m  = noEff m { currentEntry = Just kv }
updateModel Add m@Model{..} =
  let (k,desc) = fromJust currentEntry
      (k', m') = updateEntry k desc m
  in m' <# do
    focus $ "item-" <> S.ms k'
    pure NoOp
updateModel Delete m@Model{..} = noEff $
  deleteEntry (fst $ fromJust currentEntry) m

nextKey :: IntMap a -> Int
nextKey = succ . fst . fst . fromJust . Map.maxViewWithKey

enableEditing :: Item -> Item
enableEditing item = item { editing = True }

disablePrevEditing :: Maybe (Map.Key, MisoString) -> IntMap Item -> IntMap Item
disablePrevEditing mbEntry items =
  maybe items (\(k, v) -> Map.alter (const (mbItem v)) k items) mbEntry
    where
      mbItem v = bool (Just $ Item v False) Nothing $ S.null v

updateEntry :: Map.Key -> MisoString -> Model -> (Map.Key, Model)
updateEntry k desc m@Model{..} =
  let items = Map.update (\item -> Just item { itemDescription = desc
                                             , editing = False
                                             }
                         ) k allEntries
      k' = nextKey items
      newItem = Map.singleton k'  $ Item "" True
  in ( k'
     , m { allEntries = items <> newItem
         , currentEntry = Just (k', "")
         }
     )

deleteEntry :: Map.Key -> Model -> Model
deleteEntry k m@Model{..} =
  let items = Map.delete k allEntries
      m' = m { allEntries = items , currentEntry = Nothing }
  in bool m' initialModel $ Map.null items

viewModel :: Model -> View Action
viewModel Model{..} = div_ [ id_ "contentWrapper" ]
  [ h1_ []
      [ text "Just "
      , em_ [] [ text "To Do" ]
      , text " It, Basically"
      ]
  , article_ [ class_ "post" ]
      [ h2_ [] [ text "Enter and edit your list at will" ] ]
  , article_ []
      [ div_ [ id_ "container" ]
          [ ul_ [ id_ "theList" ] $
              Map.foldrWithKey' (\k item@Item{..} xs ->
                entry k (curDesc itemDescription) item : xs) [] allEntries
          ]
      ]
  ]
  where
    curDesc desc = fromMaybe desc $ fmap snd currentEntry

entry :: Map.Key -> MisoString -> Item -> View Action
entry k desc Item{..} = li_
  [ onClick (Edit k) ]
  [ bool (text itemDescription) (itemInput k desc) editing
  ]

itemInput :: Map.Key -> MisoString -> View Action
itemInput k desc = input_
  [ id_ $ "item-" <> S.ms k
  , autofocus_ True
  , onInput (Input . (k,))
  , value_ desc
  , onKeyDown (bool NoOp (addOrDelete desc) . (== KeyCode 13))
  ]

addOrDelete :: MisoString -> Action
addOrDelete = bool Add Delete . S.null
