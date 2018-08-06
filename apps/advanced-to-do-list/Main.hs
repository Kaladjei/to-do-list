{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Miso
import           Miso.String (MisoString, ms)
import qualified Miso.String as S

type Model = MisoString

initialModel :: Model
initialModel = ""

data Action
  = NoOp

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
updateModel _ = noEff


viewModel :: Model -> View Action
viewModel model = div_ [] [ text "Everything will go here" ]
