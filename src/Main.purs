module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.MouseEvent (eventToMouseEvent, pageX, pageY)
import DOM.Event.Types (Event)
import DOM.HTML as H
import DOM.HTML.Event.EventTypes (mousemove)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document, requestAnimationFrame)
import DOM.Node.ParentNode (QuerySelector(QuerySelector), querySelector)
import DOM.Node.Types (Element, ParentNode, elementToEventTarget)
import Data.AppState (AppState(..), initialAppState)
import Data.Either (either)
import Data.Int (toNumber)
import Data.Maybe (Maybe, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS, Context2D, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, strokePath)

-- Milestones
-- - While mousedown, draw line from 0,0 to the cursor, random colour rAF

type AppEff u = forall eff a. Eff (
    dom     :: DOM
  , console :: CONSOLE
  , canvas  :: CANVAS
  , st      :: ST a
  | eff
  ) u

onCanvasEvent :: forall eff a
  . STRef a AppState
 -> Event
 -> Eff (
      dom     :: DOM
    , console :: CONSOLE
    , canvas  :: CANVAS
    , st      :: ST a
    | eff
    ) Unit
onCanvasEvent str event = either logError handleEvent e where
  e              = runExcept (eventToMouseEvent event)
  logError    _  = log "Failed to use mouse event"
  handleEvent me = do
    _ <- writeSTRef str (AppState { mouseX : pageX me, mouseY : pageY me })
    pure unit

--------------------------------------------------------------------------------

loop :: forall eff a
   . STRef a AppState
  -> Context2D
  -> Eff (
    dom     :: DOM
  , console :: CONSOLE
  , canvas  :: CANVAS
  , st      :: ST a
  | eff
  ) Unit
loop str ctx = do

  AppState s <- readSTRef str
  log (show (AppState s))

  _ <- setFillStyle "#0000ff" ctx
  _ <- strokePath ctx $ do
    _ <- moveTo ctx 0.0 0.0
    lineTo ctx (toNumber s.mouseX) (toNumber s.mouseY)

  win <- H.window
  _ <- requestAnimationFrame (loop str ctx) win
  pure unit

-------------------------------------------------------------------------------

bindCanvas :: Tuple Element Context2D -> AppEff Unit
bindCanvas (Tuple canvasEl ctx) = do

  str <- newSTRef initialAppState

  let listener = eventListener (onCanvasEvent str)
      target   = elementToEventTarget canvasEl

  addEventListener mousemove listener true target

  loop str ctx

-------------------------------------------------------------------------------

canvasSelector :: forall eff. ParentNode -> Eff (dom :: DOM | eff) (Maybe Element)
canvasSelector = querySelector (QuerySelector "#c")

-------------------------------------------------------------------------------

main :: AppEff Unit
main = do

  el  <- H.window >>= document >>= (canvasSelector <<< htmlDocumentToParentNode)
  el' <- getCanvasElementById "c"
  ctx <- sequence (getContext2D <$> el')

  maybe (log "Could not find canvas") bindCanvas (lift2 Tuple el ctx)
