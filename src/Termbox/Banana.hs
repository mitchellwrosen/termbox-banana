{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Termbox.Banana
  ( -- * Introduction
    -- $intro

    -- * Core API
    TermboxEvent,
    run,

    -- * Re-exports from @termbox@
    Termbox.black,
    Termbox.blue,
    Termbox.bold,
    Termbox.cyan,
    Termbox.green,
    Termbox.magenta,
    Termbox.red,
    Termbox.reverse,
    Termbox.underline,
    Termbox.white,
    Termbox.yellow,
    Termbox.set,
    Termbox.Attr,
    Termbox.Cell (..),
    Termbox.Cells,
    Termbox.Cursor (..),
    Termbox.Event (..),
    Termbox.InitError (..),
    Termbox.Key (..),
    pattern Termbox.KeyCtrl2,
    pattern Termbox.KeyCtrl3,
    pattern Termbox.KeyCtrl4,
    pattern Termbox.KeyCtrl5,
    pattern Termbox.KeyCtrl7,
    pattern Termbox.KeyCtrlH,
    pattern Termbox.KeyCtrlI,
    pattern Termbox.KeyCtrlLsqBracket,
    pattern Termbox.KeyCtrlM,
    pattern Termbox.KeyCtrlUnderscore,
    Termbox.Mouse (..),
    Termbox.PollError (..),
  )
where

import Control.Concurrent.MVar
import Data.Function (fix)
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Termbox

-- $intro
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Termbox.Banana as Termbox
-- @
--
-- ==== __👉 Quick start example__
--
-- This is a program that displays the last key pressed, and quits on @Esc@:
--
-- @
-- {-\# LANGUAGE LambdaCase          \#-}
-- {-\# LANGUAGE ScopedTypeVariables \#-}
--
-- module Main where
--
-- import Reactive.Banana
-- import Reactive.Banana.Frameworks
--
-- import qualified Termbox.Banana as Termbox
--
-- main :: IO ()
-- main =
--   Termbox.'run' moment
--
-- moment
--   :: Event Termbox.'Termbox.Event'
--   -> Behavior (Int, Int)
--   -> MomentIO (Behavior (Termbox.'Termbox.Cells', Termbox.'Termbox.Cursor'), Event ())
-- moment eEvent _bSize = do
--   let
--     eQuit :: Event ()
--     eQuit =
--       () <$ filterE isKeyEsc eEvent
--
--   bLatestEvent :: Behavior (Maybe Termbox.'Termbox.Event') <-
--     stepper
--       Nothing
--       (Just \<$\> eEvent)
--
--   let
--     bCells :: Behavior Termbox.'Termbox.Cells'
--     bCells =
--       maybe mempty renderEvent \<$\> bLatestEvent
--
--   let
--     bScene :: Behavior (Termbox.'Termbox.Cells', Termbox.'Termbox.Cursor')
--     bScene =
--       (,)
--         \<$\> bCells
--         \<*\> pure Termbox.'Termbox.NoCursor'
--
--   pure (bScene, eQuit)
--
-- renderEvent :: Termbox.'Termbox.Event' -> Termbox.'Termbox.Cells'
-- renderEvent =
--   foldMap (\\(i, c) -> Termbox.set i 0 (Termbox.'Termbox.Cell' c mempty mempty))
--     . zip [0..]
--     . show
--
-- isKeyEsc :: Termbox.'Termbox.Event' -> Bool
-- isKeyEsc = \\case
--   Termbox.'Termbox.EventKey' Termbox.'Termbox.KeyEsc' -> True
--   _ -> False
-- @

-- | A @termbox@ event. This type alias exists only for Haddock readability;
-- in code, you are encouraged to use
--
-- * @Event@ for @reactive-banana@ events
-- * @Termbox.Event@ for @termbox@ events
type TermboxEvent =
  Termbox.Event

type EventSource a =
  (AddHandler a, a -> IO ())

-- | Run a @termbox@ program with the specified input and output modes.
--
-- Given
--
-- * the terminal event stream
-- * the time-varying terminal size (width, then height)
--
-- return
--
-- * a time-varying scene to render
-- * an event stream of arbitrary values, only the first of which is relevant,
--   which ends the @termbox@ program and returns from the @main@ action.
run ::
  ( Event TermboxEvent ->
    Behavior (Int, Int) ->
    MomentIO (Behavior (Termbox.Cells, Termbox.Cursor), Event a)
  ) ->
  IO a
run program =
  Termbox.run $ \width height render poll -> do
    doneVar :: MVar a <-
      newEmptyMVar

    (eventAddHandler, fireEvent) :: EventSource TermboxEvent <-
      newAddHandler

    network :: EventNetwork <-
      compile $ do
        eEvent :: Event TermboxEvent <-
          fromAddHandler eventAddHandler

        let eResize :: Event (Int, Int)
            eResize =
              filterJust
                ( ( \case
                      Termbox.EventResize w h -> Just (w, h)
                      _ -> Nothing
                  )
                    <$> eEvent
                )

        bSize :: Behavior (Int, Int) <-
          flip stepper eResize (width, height)

        moment (uncurry render) program eEvent bSize (putMVar doneVar)

    actuate network

    fix $ \loop -> do
      poll >>= fireEvent
      tryReadMVar doneVar >>= maybe loop pure

moment ::
  ((Termbox.Cells, Termbox.Cursor) -> IO ()) ->
  ( Event TermboxEvent ->
    Behavior (Int, Int) ->
    MomentIO (Behavior (Termbox.Cells, Termbox.Cursor), Event a)
  ) ->
  Event TermboxEvent ->
  Behavior (Int, Int) ->
  (a -> IO ()) ->
  MomentIO ()
moment render program eEvent bSize abort = do
  (bScene, eDone) :: (Behavior (Termbox.Cells, Termbox.Cursor), Event a) <-
    program eEvent bSize

  eScene :: Event (Future (Termbox.Cells, Termbox.Cursor)) <-
    changes bScene

  liftIO . render =<< valueB bScene
  reactimate (abort <$> eDone)
  reactimate' ((fmap . fmap) render eScene)
