{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Termbox.Banana as Termbox

main :: IO ()
main =
  Termbox.run_
    (Termbox.InputModeEsc Termbox.MouseModeNo)
    Termbox.OutputModeNormal
    moment

moment ::
  Event Termbox.Event ->
  Behavior (Int, Int) ->
  MomentIO (Behavior Termbox.Scene, Event ())
moment eEvent _bSize = do
  let eQuit :: Event ()
      eQuit =
        () <$ filterE isKeyEsc eEvent

  bLatestEvent :: Behavior (Maybe Termbox.Event) <-
    stepper
      Nothing
      (Just <$> eEvent)

  let bCells :: Behavior Termbox.Cells
      bCells =
        maybe mempty renderEvent <$> bLatestEvent

  let bScene :: Behavior Termbox.Scene
      bScene =
        Termbox.Scene
          <$> bCells
          <*> pure Termbox.NoCursor

  pure (bScene, eQuit)

renderEvent :: Termbox.Event -> Termbox.Cells
renderEvent =
  foldMap (\(i, c) -> Termbox.set i 0 (Termbox.Cell c mempty mempty))
    . zip [0 ..]
    . show

isKeyEsc :: Termbox.Event -> Bool
isKeyEsc = \case
  Termbox.EventKey Termbox.KeyEsc _ -> True
  _ -> False
