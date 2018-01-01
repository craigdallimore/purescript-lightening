module Data.AppState where

import Prelude

import Data.Foldable (foldr)

newtype AppState = AppState (Array Point)

newtype Point = Point { x :: Int, y :: Int }

instance showPoint :: Show Point where
  show (Point s) = "x: " <> show s.x <> ", y: " <> show s.y

instance showAppState :: Show AppState where
  show (AppState xs) = foldr (\p acc -> acc <> "\n" <> show p) "" xs

initialAppState :: AppState
initialAppState = AppState []
