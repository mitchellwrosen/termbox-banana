{-# language DerivingStrategies         #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase                 #-}
{-# language ScopedTypeVariables        #-}
{-# language TypeFamilies               #-}

module Termbox.Banana
  ( TermboxEvent
  , main
  , Scene
  , set
  , done
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

-- | The scene to render.
data Scene a
  = SceneDone a
  | SceneDraw (IO ())

instance Monoid (Scene a) where
  mempty = SceneDraw mempty
  mappend = (<>)

instance Semigroup (Scene a) where
  SceneDone x <> _ = SceneDone x
  _ <> SceneDone x = SceneDone x
  SceneDraw x <> SceneDraw y = SceneDraw (x <> y)

done :: a -> Scene a
done =
  SceneDone

-- | Set a cell's value.
set :: (col ~ Int, row ~ Int) => col -> row -> Termbox.Cell -> Scene a
set x y z =
  SceneDraw (Termbox.set x y z)

type EventSource a
  = (AddHandler a, a -> IO ())

main
  :: (width ~ Int, height ~ Int)
  => (  Event TermboxEvent
     -> Behavior (width, height)
     -> MomentIO (Behavior (Scene a)))
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
     -> MomentIO (Behavior (Scene a)))
  -> Event TermboxEvent
  -> Behavior (width, height)
  -> (a -> IO ())
  -> MomentIO ()
moment run eEvent bSize abort = do
  bScene :: Behavior (Scene a) <-
    run eEvent bSize

  eScene :: Event (Future (Scene a)) <-
    changes bScene

  let
    render :: Scene a -> IO ()
    render = \case
      SceneDone x ->
        abort x
      SceneDraw m -> do
        Termbox.clear mempty mempty
        m
        Termbox.flush

  liftIO . render =<< valueB bScene
  reactimate' ((fmap.fmap) render eScene)
