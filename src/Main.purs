module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Except(runExcept)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.MouseEvent (eventToMouseEvent, screenX, screenY)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (click)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(QuerySelector), querySelector)
import DOM.Node.Types (Element, ParentNode, elementToEventTarget)
import Data.Maybe (Maybe, maybe)
import Data.Either (Either(Right))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS, Context2D, closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle)

-- Milestones
-- - Onclick, draw a line from 0,0 to the cursor
-- - While mousedown, draw line from 0,0 to the cursor, random colour rAF

canvasClickListener :: forall eff
  . Event
 -> Eff (
      dom     :: DOM
    , console :: CONSOLE
    | eff
    ) Unit
canvasClickListener event = case runExcept (eventToMouseEvent event) of
  Right me -> log $ "x: " <> show (screenX me) <> ", y: " <> show (screenY me)
  _        -> log "Failed to use mouse event"

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

  addEventListener click (eventListener canvasClickListener) true (elementToEventTarget canvasEl)

  _ <- setFillStyle "#ff0000" ctx

  _ <- fillPath ctx $ do
    _ <- moveTo ctx 10.0 10.0
    _ <- lineTo ctx 20.0 20.0
    _ <- lineTo ctx 10.0 20.0
    closePath ctx

  pure unit

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
