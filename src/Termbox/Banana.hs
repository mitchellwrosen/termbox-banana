{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Termbox.Banana
  ( -- $intro
    TermboxEvent,
    run,
    run_,
    set,
    Scene (..),
    Cells,
    Cursor (..),

    -- * Re-exports
    Termbox.black,
    Termbox.red,
    Termbox.green,
    Termbox.yellow,
    Termbox.blue,
    Termbox.magenta,
    Termbox.cyan,
    Termbox.white,
    Termbox.bold,
    Termbox.underline,
    Termbox.reverse,
    Termbox.Attr,
    Termbox.Cell (..),
    Termbox.Event (..),
    Termbox.InitError (..),
    Termbox.InputMode (..),
    Termbox.Key (..),
    Termbox.Mouse (..),
    Termbox.MouseMode (..),
    Termbox.OutputMode (..),

    -- * Example
    -- $example
  )
where

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Data.Function (fix)
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Termbox

-- $intro
-- See the bottom of this module for a simple, runnable example to get started.
--
-- Here's how to run the examples with @cabal@:
--
-- @
-- cabal v2-run --constraint "termbox-banana +build-examples" termbox-banana-example-echo
-- cabal v2-run --constraint "termbox-banana +build-examples" termbox-banana-example-hoogle
-- @
--
-- This module is intended to be imported qualified.
--
-- @
-- import qualified Termbox.Banana as Termbox
-- @

-- | A @termbox@ event. This type alias exists only for Haddock readability;
-- in code, you are encouraged to use
--
-- * @Event@ for @reactive-banana@ events
-- * @Termbox.Event@ for @termbox@ events
--
-- @since 0.1.0
type TermboxEvent =
  Termbox.Event

-- | A scene to render; a grid of cells and a cursor.
--
-- @since 0.1.0
data Scene
  = Scene !Cells !Cursor

-- | A grid of cells.
--
-- Create a 'Cells' with 'set' or 'mempty' and combine them with ('<>').
--
-- @since 0.1.0
newtype Cells
  = Cells (IO ())
  deriving newtype (Monoid, Semigroup)

-- | A cursor.
--
-- @since 0.1.0
data Cursor
  = -- | Column, then row
    Cursor !Int !Int
  | NoCursor

-- | Set a single cell's value (column, then row).
--
-- @since 0.1.0
set :: Int -> Int -> Termbox.Cell -> Cells
set x y z =
  Cells (Termbox.set x y z)

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
--
-- @since 0.2.0
run ::
  -- |
  Termbox.InputMode ->
  -- |
  Termbox.OutputMode ->
  ( Event TermboxEvent ->
    Behavior (Int, Int) ->
    MomentIO (Behavior Scene, Event a)
  ) ->
  IO (Either Termbox.InitError a)
run imode omode program =
  Termbox.run $ do
    Termbox.setInputMode imode
    Termbox.setOutputMode omode

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
          flip stepper eResize
            =<< liftIO Termbox.getSize

        moment program eEvent bSize (putMVar doneVar)

    actuate network

    fix $ \loop -> do
      Termbox.poll >>= fireEvent
      tryReadMVar doneVar
        >>= maybe loop pure

-- | Like 'run', but throws 'Termbox.InitError's as @IO@ exceptions.
--
-- @since 0.2.0
run_ ::
  -- |
  Termbox.InputMode ->
  -- |
  Termbox.OutputMode ->
  ( Event TermboxEvent ->
    Behavior (Int, Int) ->
    MomentIO (Behavior Scene, Event a)
  ) ->
  IO a
run_ imode omode program =
  run imode omode program >>= either throwIO pure

moment ::
  ( Event TermboxEvent ->
    Behavior (Int, Int) ->
    MomentIO (Behavior Scene, Event a)
  ) ->
  Event TermboxEvent ->
  Behavior (Int, Int) ->
  (a -> IO ()) ->
  MomentIO ()
moment program eEvent bSize abort = do
  (bScene, eDone) :: (Behavior Scene, Event a) <-
    program eEvent bSize

  eScene :: Event (Future Scene) <-
    changes bScene

  let render :: Scene -> IO ()
      render (Scene (Cells cells) cursor) = do
        Termbox.clear mempty mempty
        cells
        case cursor of
          Cursor c r -> Termbox.setCursor c r
          NoCursor -> Termbox.hideCursor
        Termbox.flush

  liftIO . render =<< valueB bScene
  reactimate (abort <$> eDone)
  reactimate' ((fmap . fmap) render eScene)

-- $example
--
-- Below is a sample program that simply displays the last key pressed, and
-- quits on @Esc@:
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
--   Termbox.'run_'
--     (Termbox.'Termbox.InputModeEsc' Termbox.'Termbox.MouseModeNo')
--     Termbox.'Termbox.OutputModeNormal'
--     moment
--
-- moment
--   :: Event Termbox.'Termbox.Event'
--   -> Behavior (Int, Int)
--   -> MomentIO (Behavior Termbox.'Termbox.Scene', Event ())
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
--     bScene :: Behavior Termbox.'Termbox.Scene'
--     bScene =
--       Termbox.'Termbox.Scene'
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
--   Termbox.'Termbox.EventKey' Termbox.'Termbox.KeyEsc' _ -> True
--   _ -> False
-- @
