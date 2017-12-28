module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.MouseEvent (eventToMouseEvent, pageX, pageY)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (click)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(QuerySelector), querySelector)
import DOM.Node.Types (Element, ParentNode, elementToEventTarget)
import Data.Either (either)
import Data.Int (toNumber)
import Data.Maybe (Maybe, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS, Context2D, strokePath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle)

-- Milestones
-- - While mousedown, draw line from 0,0 to the cursor, random colour rAF

onCanvasClick :: forall eff
  . Context2D
 -> Event
 -> Eff (
      dom     :: DOM
    , console :: CONSOLE
    , canvas  :: CANVAS
    | eff
    ) Unit
onCanvasClick ctx event = either logError handleClick (runExcept (eventToMouseEvent event)) where
  logError    _  = log "Failed to use mouse event"
  handleClick me = do
    log $ "x: " <> show x <> ", y: " <> show y
    _ <- setFillStyle "#0000ff" ctx
    _ <- strokePath ctx $ do
      _ <- moveTo ctx 0.0 0.0
      lineTo ctx (toNumber x) (toNumber y)
    pure unit
    where
      x = pageX me
      y = pageY me


bindCanvas
  :: forall eff
   . Tuple Element Context2D
  -> Eff (
        dom     :: DOM
      , canvas  :: CANVAS
      , console :: CONSOLE
      | eff
      ) Unit
bindCanvas (Tuple canvasEl ctx) = do

  let listener = eventListener (onCanvasClick ctx)
      target   = elementToEventTarget canvasEl

  addEventListener click listener true target

canvasSelector :: forall eff. ParentNode -> Eff (dom :: DOM | eff) (Maybe Element)
canvasSelector = querySelector (QuerySelector "#c")

main
  :: forall eff
   . Eff (
      dom     :: DOM
    , console :: CONSOLE
    , canvas  :: CANVAS
    | eff
    ) Unit
main = do

  el  <- window >>= document >>= (canvasSelector <<< htmlDocumentToParentNode)
  el' <- getCanvasElementById "c"
  ctx <- sequence (getContext2D <$> el')

  maybe (log "Could not find canvas") bindCanvas (lift2 Tuple el ctx)
