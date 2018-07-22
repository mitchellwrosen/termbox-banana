{-# language DerivingStrategies         #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase                 #-}
{-# language ScopedTypeVariables        #-}
{-# language TypeFamilies               #-}

module Termbox.Banana
  ( TermboxEvent
  , Scene(..)
  , Cells(..)
  , Cursor(..)
  , main
  , set
    -- * Re-exports
  , Termbox.Cell(..)
  , Termbox.Event(..)
  , Termbox.InitError(..)
  , Termbox.Key(..)
  ) where

import Control.Concurrent.STM
import Data.Function (fix)
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Termbox

-- | A @termbox@ event. This type alias exists only for Haddock readability;
-- in code, you are encouraged to use
--
-- * @Event@ for @reactive-banana@ events
-- * @Termbox.Event@ for @termbox@ events
type TermboxEvent
  = Termbox.Event

data Scene
  = Scene !Cells !Cursor

newtype Cells
  = Cells (IO ())
  deriving newtype (Monoid, Semigroup)

data Cursor
  = Cursor !Int !Int -- ^ Column, then row
  | NoCursor

-- | Set a cell's value.
set :: (col ~ Int, row ~ Int) => col -> row -> Termbox.Cell -> Cells
set x y z =
  Cells (Termbox.set x y z)

type EventSource a
  = (AddHandler a, a -> IO ())

main
  :: (width ~ Int, height ~ Int)
  => (  Event TermboxEvent
     -> Behavior (width, height)
     -> MomentIO (Behavior (Either a Scene)))
  -> IO a
main run =
  Termbox.main $ do
    doneVar :: TMVar a <-
      newEmptyTMVarIO

    (eventAddHandler, fireEvent) :: EventSource TermboxEvent <-
      newAddHandler

    network :: EventNetwork <-
      compile $ do
        eEvent :: Event TermboxEvent <-
          fromAddHandler eventAddHandler

        let
          eResize :: Event (Int, Int)
          eResize =
            filterJust
              ((\case
                Termbox.EventResize w h -> Just (w, h)
                _ -> Nothing)
              <$> eEvent)

        bSize :: Behavior (Int, Int) <- do
          flip stepper eResize =<<
            liftIO Termbox.size

        moment run eEvent bSize (atomically . putTMVar doneVar)

    actuate network

    fix $ \loop -> do
      Termbox.poll >>= fireEvent
      atomically (tryReadTMVar doneVar) >>=
        maybe loop pure

moment
  :: (width ~ Int, height ~ Int)
  => (  Event TermboxEvent
     -> Behavior (width, height)
     -> MomentIO (Behavior (Either a Scene)))
  -> Event TermboxEvent
  -> Behavior (width, height)
  -> (a -> IO ())
  -> MomentIO ()
moment run eEvent bSize abort = do
  bScene :: Behavior (Either a Scene) <-
    run eEvent bSize

  eScene :: Event (Future (Either a Scene)) <-
    changes bScene

  let
    render :: Either a Scene -> IO ()
    render =
      either
        abort
        (\(Scene (Cells cells) cursor) -> do
          Termbox.clear mempty mempty
          cells
          case cursor of
            Cursor c r -> Termbox.setCursor c r
            NoCursor -> Termbox.hideCursor
          Termbox.flush)

  liftIO . render =<< valueB bScene
  reactimate' ((fmap.fmap) render eScene)
