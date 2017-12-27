module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (click)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(QuerySelector), querySelector)
import DOM.Node.Types (Element, ParentNode, elementToEventTarget)
import Data.Maybe (Maybe, maybe)
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
canvasClickListener e = log "clicked"

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

  mCanvEl  <- window >>= document >>= (canvasSelector <<< htmlDocumentToParentNode)

  mEl      <- getCanvasElementById "c"
  maybeCtx <- sequence (getContext2D <$> mEl)

  maybe (log "Could not find canvas") bindCanvas (lift2 Tuple mCanvEl maybeCtx)
