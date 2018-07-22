{-# language LambdaCase          #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies        #-}

module Termbox.Banana
  ( TermboxEvent
  , main
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Void
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Termbox

type TermboxEvent
  = Termbox.Event

type EventSource a
  = (AddHandler a, a -> IO ())

main :: (Event TermboxEvent -> MomentIO (Behavior (Either a (IO ())))) -> IO a
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

        moment run eEvent (atomically . putTMVar doneVar)
    actuate network

    let
      thread1 :: IO Void
      thread1 =
        forever (Termbox.poll >>= fireEvent)

    let
      thread2 :: IO a
      thread2 =
        atomically (readTMVar doneVar)

    race thread1 thread2 >>=
      either absurd pure

moment
  :: (Event TermboxEvent -> MomentIO (Behavior (Either a (IO ()))))
  -> Event TermboxEvent
  -> (a -> IO ())
  -> MomentIO ()
moment run eEvent done = do
  bWrite :: Behavior (Either a (IO ())) <-
    run eEvent

  eWrite :: Event (Future (Either a (IO ()))) <-
    changes bWrite

  let
    render :: IO () -> IO ()
    render act =
      Termbox.clear mempty mempty *> act <* Termbox.flush

  (`whenRight` liftIO . render) =<<
    valueB bWrite

  reactimate' ((fmap.fmap) (either done render) eWrite)

whenRight :: Monad m => Either a b -> (b -> m ()) -> m ()
whenRight x f =
  case x of
    Left _ -> pure ()
    Right y -> f y
