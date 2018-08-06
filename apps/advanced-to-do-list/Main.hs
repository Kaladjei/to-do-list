{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Miso
import           Miso.String (MisoString, ms)
import qualified Miso.String as S

import           Control.Arrow (first)
import           Data.Aeson
import           Data.Bool
import           Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics

data Item = Item
  { itemDescription :: MisoString
  , editing         :: Bool
  , draggable       :: Bool
  , showDelete      :: Bool
  } deriving (Eq, Generic, ToJSON, FromJSON)

data Model = Model
  { allEntries   :: IntMap Item
  , currentEntry :: MisoString
  , editEntry    :: Maybe (Map.Key, MisoString)
  , draggedItem  :: Maybe (Map.Key, Item)
  } deriving (Eq, Generic, ToJSON, FromJSON)

initialModel :: Model
initialModel = Model Map.empty "" Nothing Nothing

data Action
  = NoOp
  | Add
  | LoadToDo
  | SetToDo Model
  | Input MisoString
  | Edit Map.Key
  | EditInput (Map.Key, MisoString)
  | EditDone
  | Delete Map.Key
  | ShowDelete Map.Key
  | HideDelete Map.Key
  | ClearAll
  | Draggable (Map.Key, Bool)
  | Drag Map.Key
  | DraggedOver Map.Key
  | Drop

main :: IO ()
main = startApp App {..}
  where
    initialAction = LoadToDo
    model         = initialModel
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    subs          = []
    mountPoint    = Nothing

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel LoadToDo m = m <# do
  store <- getLocalStorage "advancedTodoList"
  pure $ SetToDo $ either (const m) id store
updateModel (SetToDo m) _ = m <# focusToDoItem
updateModel Add m@Model{..} =
  let items = bool (addItem currentEntry allEntries) allEntries $ S.null currentEntry
      m' = m { allEntries = items
             , currentEntry = ""
             }
  in m' <# do
    setLocalStorage "advancedTodoList" m'
    focus "toDoItem"
    pure NoOp
updateModel (Input desc) m = noEff m { currentEntry = desc }
updateModel (Edit k) m@Model{..} =
  m { allEntries = disablePrevEditing editEntry $
                     Map.update (Just . enableEditing) k allEntries
    , editEntry = Just (k , itemDescription $ allEntries Map.! k)
    } <# do
    focus $ "item-" <> S.ms k
    pure NoOp
updateModel (EditInput kv) m  = noEff m { editEntry = Just kv }
updateModel EditDone m@Model{..} =
  let m' = uncurry updateEntry (fromMaybe (0,"") editEntry) m
  in m' <# (setLocalStorage "advancedTodoList" m' >> focusToDoItem)
updateModel (Delete k) m@Model{..} =
  let m' = m { allEntries = Map.delete k allEntries }
  in m' <# (setLocalStorage "advancedTodoList" m' >> focusToDoItem)
updateModel (ShowDelete k) m@Model{..} = noEff
  m { allEntries = Map.update (\item -> Just $ item { showDelete = True }) k allEntries }
updateModel (HideDelete k) m@Model{..} = noEff
  m { allEntries = Map.update (\item -> Just $ item { showDelete = False }) k allEntries }
updateModel ClearAll _ = initialModel <#
  (setLocalStorage "advancedTodoList" initialModel >> focusToDoItem)
updateModel (Draggable (k, b)) m@Model{..} = noEff
  m { allEntries = Map.update (\item -> Just $ item { draggable = b }) k allEntries }
updateModel (Drag k) m@Model{..} = noEff
  m { draggedItem = fmap (k,) $ Map.lookup k allEntries
    , allEntries = Map.delete k allEntries
    }
updateModel (DraggedOver k) m@Model{..}  =
  let Just (k', item') = draggedItem
      item = allEntries Map.! k
      items =  Map.fromList [(k, item'), (k', item)] <> allEntries
      draggedItem' = first (const k) <$> draggedItem
      m' = m { allEntries = items
             , draggedItem = draggedItem'
             }
  in m' <# (setLocalStorage "advancedTodoList" m' >> pure NoOp)
updateModel Drop m@Model{..} = noEff m { draggedItem = Nothing }

focusToDoItem :: IO Action
focusToDoItem = do
  focus "toDoItem"
  pure NoOp

addItem :: MisoString -> IntMap Item -> IntMap Item
addItem desc items =
  let newItem = Item desc False False False
      k = fromMaybe 1 $ (succ . fst . fst)  <$> Map.maxViewWithKey items
  in Map.insert k newItem items

enableEditing :: Item -> Item
enableEditing item = item { editing = True }

disablePrevEditing :: Maybe (Map.Key, MisoString) -> IntMap Item -> IntMap Item
disablePrevEditing mbEntry items =
  maybe items (\(k, v) -> Map.alter (const (mbItem v)) k items) mbEntry
    where
      mbItem v = bool (Just $ Item v False False False) Nothing $ S.null v

disableEditing :: Item -> Item
disableEditing item@Item{..} =
  bool item (item { editing = False }) editing

updateEntry :: Map.Key -> MisoString -> Model -> Model
updateEntry k "" m@Model{..} =
  m { allEntries = Map.update (Just. disableEditing) k allEntries
    , editEntry = Nothing
    }
updateEntry k desc m@Model{..} =
  let items = Map.update (\item -> Just item { itemDescription = desc
                                             , editing = False
                                             }
                         ) k allEntries
  in m { allEntries = items
       , editEntry = Nothing
       }

viewModel :: Model -> View Action
viewModel Model{..} = div_ [ id_ "contentWrapper" ]
  [ h1_ []
      [ text "Just "
      , em_ [] [ text "To Do" ]
      , text " It, Basically"
      ]
  , article_ [ class_ "post" ]
      [ h2_ [] [ text "Keep track of your activities." ] ]
  , div_ [ id_ "container" ] $
      [ div_ [ id_ "form1" ]
          [ input_
              [ type_ "text"
              , name_ "toDoItem"
              , id_ "toDoItem"
              , value_ currentEntry
              , autofocus_ True
              , onInput Input
              , onKeyDown (bool NoOp Add . (== KeyCode 13))
              ]
          , button_
              [ id_ "addToDo"
              , disabled_ $ S.null currentEntry
              , onClick Add
              ]
              [ text "Add List Item" ]
          ]
      , ul_ [ id_ "theList", class_ "sortable list" ] $
          Map.foldrWithKey' (\k item@Item{..} xs ->
                listItem k (curDesc itemDescription) item showDelete draggable : xs) [] allEntries
      ] ++ conditionalViews (Map.size allEntries > 1)
            [ p_ []
                [ button_ [ id_ "clearAll", onClick ClearAll ] [ text "Clear All" ]
                ]
            ]
  ]
  where
    curDesc desc = fromMaybe desc $ fmap snd editEntry

listItem :: Map.Key -> MisoString -> Item -> Bool -> Bool -> View Action
listItem k desc Item{..} b drag = li_
  [ boolProp "draggable" drag
  , onDragStart (Drag k)
  , onDragOver (DraggedOver k)
  , onDrop (AllowDrop True) Drop
  ]
  [ span_
      [ id_ "drag"
      , onMouseOver (Draggable (k, True))
      , onMouseOut (Draggable (k, False))
      ] [ text "::" ]
  , span_
      [ onMouseOver (ShowDelete k)
      , onMouseOut (HideDelete k)
      ] $
      [ bool (itemDisplay k itemDescription) (itemInput k desc) editing
      ] ++ conditionalViews b
             [ button_
                 [ class_ "removeListItem"
                 , onClick (Delete k)
                 ]
                 [ text "X" ]
             ]
  ]

itemDisplay :: Map.Key -> MisoString -> View Action
itemDisplay k desc = span_ [ onClick (Edit k) ] [ text desc ]

itemInput :: Map.Key -> MisoString -> View Action
itemInput k desc = input_
  [ id_ $ "item-" <> S.ms k
  , autofocus_ True
  , onInput (EditInput . (k,))
  , value_ desc
  , onKeyDown (bool NoOp EditDone . (== KeyCode 13))
  , onBlur EditDone
  ]
