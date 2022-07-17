{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Termbox.Banana
  ( -- * Introduction
    -- $intro

    -- * Core API
    Event (..),
    Program,
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
import Control.Monad.IO.Class (liftIO)
import Data.Function (fix)
import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana
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
-- {-\# LANGUAGE LambdaCase \#-}
--
-- module Main where
--
-- import Data.Void (Void)
-- import Reactive.Banana
-- import Reactive.Banana.Frameworks
-- import qualified Termbox.Banana as Termbox
--
-- main :: IO ()
-- main =
--   Termbox.'run' moment
--
-- moment
--   :: (Void -> IO ())
--   -> Event (Termbox.'Event' Void)
--   -> Behavior (Int, Int)
--   -> MomentIO (Behavior (Termbox.'Termbox.Cells', Termbox.'Termbox.Cursor'), Event ())
-- moment _fireUserEvent eEvent _bSize = do
--   let eQuit = () <$ filterE isKeyEsc eEvent
--   bLatestEvent <- stepper Nothing (Just \<$\> eEvent)
--   let bCells = maybe mempty renderEvent \<$\> bLatestEvent
--   let bScene = (,) \<$\> bCells \<*\> pure Termbox.'Termbox.NoCursor'
--   pure (bScene, eQuit)
--
-- renderEvent :: Show a => Termbox.'Event' a -> Termbox.'Termbox.Cells'
-- renderEvent =
--   foldMap (\\(i, c) -> Termbox.set i 0 (Termbox.'Termbox.Cell' c mempty mempty))
--     . zip [0..]
--     . show
--
-- isKeyEsc :: Termbox.'Event' a -> Bool
-- isKeyEsc = \\case
--   Termbox.'EventKey' Termbox.'Termbox.KeyEsc' -> True
--   _ -> False
-- @

-- | A key press, terminal resize, mouse click, or a user event.
data Event a
  = EventKey !Termbox.Key
  | EventResize !Int !Int
  | EventMouse !Termbox.Mouse !Int !Int
  | EventUser a
  deriving stock (Eq, Ord, Show)

type Program a b =
  -- | Callback that produces a user event.
  (a -> IO ()) ->
  -- | Event stream.
  Banana.Event (Event a) ->
  -- | Time-varying terminal size (width, then height).
  Banana.Behavior (Int, Int) ->
  -- | The time-varying scene to render, and an event stream of arbitrary values, only the first of which is relevant,
  -- which ends the @termbox@ program and returns from 'run'.
  Banana.MomentIO (Banana.Behavior (Termbox.Cells, Termbox.Cursor), Banana.Event b)

-- | Run a @termbox@ program.
run :: Program a b -> IO b
run program =
  Termbox.run $ \initialWidth initialHeight render poll -> do
    doneVar <- newEmptyMVar
    (eventAddHandler, fireEvent) <- Banana.newAddHandler
    (userEventAddHandler, fireUserEvent) <- Banana.newAddHandler

    network <-
      Banana.compile $ do
        eEvent <- Banana.fromAddHandler eventAddHandler
        eUserEvent <- Banana.fromAddHandler userEventAddHandler
        let eTermboxEvent =
              Banana.unionWith
                const
                ( ( \case
                      Termbox.EventKey key -> EventKey key
                      Termbox.EventResize width height -> EventResize width height
                      Termbox.EventMouse mouse col row -> EventMouse mouse col row
                  )
                    <$> eEvent
                )
                (EventUser <$> eUserEvent)

        let eResize :: Banana.Event (Int, Int)
            eResize =
              Banana.filterJust
                ( ( \case
                      Termbox.EventResize w h -> Just (w, h)
                      _ -> Nothing
                  )
                    <$> eEvent
                )

        bSize <- Banana.stepper (initialWidth, initialHeight) eResize
        (bScene, eDone) <- program fireUserEvent eTermboxEvent bSize
        let bRender = (\(cells, cursor) -> render cells cursor) <$> bScene
        eRender <- Banana.changes bRender
        do
          action <- Banana.valueB bRender
          liftIO action
        Banana.reactimate (putMVar doneVar <$> eDone)
        Banana.reactimate' eRender

    Banana.actuate network

    fix $ \loop -> do
      poll >>= fireEvent
      tryReadMVar doneVar >>= maybe loop pure
