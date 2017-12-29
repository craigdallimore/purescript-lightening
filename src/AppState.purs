module Data.AppState where

import Prelude

newtype AppState = AppState { mouseX :: Int , mouseY :: Int }

instance showAppState :: Show AppState where
  show (AppState s) = "x: " <> show s.mouseX <> ", y: " <> show s.mouseY

initialAppState :: AppState
initialAppState = AppState { mouseX : 0, mouseY : 0 }
