{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Foldable (fold, for_)
import Data.Maybe
import Data.String (fromString)
import Data.Text.Lens (unpacked)
import Network.HTTP.Simple
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Termbox.Banana as Termbox
import Text.HTML.TagSoup

main :: IO ()
main =
  Termbox.run moment

moment ::
  Event Termbox.Event ->
  Behavior (Int, Int) ->
  MomentIO (Behavior (Termbox.Cells, Termbox.Cursor), Event ())
moment eEvent bSize = mdo
  eTick :: Event () <-
    makeTickEvent

  requestQueue :: TQueue String <-
    liftIO newTQueueIO

  eSearchBox :: Event String <-
    makeSearchBoxEvent eKey

  reactimate ((atomically . writeTQueue requestQueue . reverse) <$> eSearchBox)

  let eEmptySearchBox :: Event String
      eEmptySearchBox =
        filterE null eSearchBox

  bSearchBox :: Behavior String <-
    stepper "" eSearchBox

  (eSearchResults, fireSearchResults) <-
    newEvent

  liftIO . void . forkIO $
    runHttpRequestThread
      requestQueue
      fireSearchResults

  bSearchResults :: Behavior (Either SomeException [Value]) <-
    stepper
      (Right [])
      ( unionWith
          const
          (Right [] <$ eEmptySearchBox)
          eSearchResults
      )

  bSpinnerFrame :: Behavior (Maybe Int) <-
    accumB
      Nothing
      ( unions
          [ ( ( \query ->
                  if null query
                    then const Nothing
                    else const (Just 1)
              )
                <$> eSearchBox
            ),
            const Nothing <$ eSearchResults,
            const Nothing <$ eEmptySearchBox,
            fmap (+ 1) <$ whenE (isJust <$> bSpinnerFrame) eTick
          ]
      )

  let bCells :: Behavior Termbox.Cells
      bCells =
        render
          <$> bHeight
          <*> bSearchBox
          <*> bSearchResults
          <*> bSpinnerFrame

  let bCursor :: Behavior Termbox.Cursor
      bCursor =
        (\height searchBox -> Termbox.Cursor (length searchBox + 2) height)
          <$> bHeight
          <*> bSearchBox

  let bScene :: Behavior (Termbox.Cells, Termbox.Cursor)
      bScene =
        (,)
          <$> bCells
          <*> bCursor

  pure (bScene, () <$ filterE (== Termbox.KeyEsc) eKey)
  where
    eKey :: Event Termbox.Key
    eKey =
      filterJust (eventAsKey <$> eEvent)
    bHeight :: Behavior Int
    bHeight =
      snd <$> bSize

makeTickEvent :: MomentIO (Event ())
makeTickEvent = do
  (e, f) <- newEvent
  liftIO . void . forkIO . forever $ do
    f ()
    threadDelay 100000
  pure e

makeSearchBoxEvent ::
  MonadMoment m =>
  Event Termbox.Key ->
  m (Event String)
makeSearchBoxEvent eKey =
  accumE
    ""
    ( unions
        [ (:) <$> filterJust (keyAsChar <$> eKey),
          (' ' :) <$ filterE (== Termbox.KeySpace) eKey,
          safeTail <$ filterE (== Termbox.KeyCtrl8) eKey
        ]
    )

runHttpRequestThread ::
  -- | Request queue
  TQueue String ->
  -- | Response callback
  (Either SomeException [Value] -> IO ()) ->
  IO ()
runHttpRequestThread requestQueue respond =
  loop Nothing
  where
    loop :: Maybe (Async [Value]) -> IO ()
    loop maybeInFlightRequest =
      join . atomically $
        ( do
            query <- readTQueue requestQueue
            pure $ do
              for_ maybeInFlightRequest $ \inFlightRequest ->
                forkIO (cancel inFlightRequest)
              if null query
                then loop Nothing
                else do
                  inFlightRequest <- async (performHoogleSearch query)
                  loop (Just inFlightRequest)
        )
          <|> ( case maybeInFlightRequest of
                  Nothing ->
                    retry
                  Just inFlightRequest -> do
                    response <- waitCatchSTM inFlightRequest
                    pure $ do
                      respond response
                      loop Nothing
              )

render ::
  Int ->
  String ->
  Either SomeException [Value] ->
  Maybe Int ->
  Termbox.Cells
render height searchBox searchResults spinnerFrame =
  fold
    [ case searchResults of
        Left ex ->
          renderSearchResultsError height ex
        Right results ->
          renderSearchResults height results
            & execWriterT
            & (`evalState` 0),
      renderSearchBox height searchBox spinnerFrame
    ]

renderSearchBox :: Int -> String -> Maybe Int -> Termbox.Cells
renderSearchBox height searchBox spinnerFrame =
  renderString 0 (height -1) (promptChar : ' ' : reverse searchBox)
  where
    promptChar :: Char
    promptChar =
      case spinnerFrame of
        Nothing ->
          'λ'
        Just n ->
          let cs =
                "⣧⣏⡟⠿⢻⣹⣼⣶"
           in cs !! (n `mod` length cs)

renderSearchResults ::
  Int ->
  [Value] ->
  WriterT Termbox.Cells (State Int) ()
renderSearchResults height = \case
  [] ->
    pure ()
  result : results -> do
    row <- get

    let ss = searchResultToLines result

    when (row + length ss < height -1) $ do
      for_ (zip [row ..] ss) $ \(r, s) ->
        tell (renderString 0 r s)
      modify' (+ (length ss + 1))
      renderSearchResults height results

renderSearchResultsError ::
  Int ->
  SomeException ->
  Termbox.Cells
renderSearchResultsError height ex =
  ex
    & show
    & lines
    & take height
    & zip [0 ..]
    & foldMap (\(row, line) -> renderString 0 row line)

searchResultToLines :: Value -> [String]
searchResultToLines result =
  case result ^?! key "type" of
    "" ->
      unwords
        [ "[" ++ resultPackage result ++ "]",
          "[" ++ resultModule result ++ "]",
          resultItem result
        ]
        : map ("  " ++) (resultDocs result)
    "module" ->
      unwords
        [ "[" ++ resultPackage result ++ "]",
          "[" ++ drop 7 (resultItem result) ++ "]"
        ]
        : map ("  " ++) (resultDocs result)
    "package" ->
      ("[" ++ drop 8 (resultItem result) ++ "]")
        : map ("  " ++) (resultDocs result)
    _ ->
      error (show result)

resultPackage :: Value -> String
resultPackage result =
  result
    ^?! key "package"
      . key "name"
      . _String
      . unpacked

resultModule :: Value -> String
resultModule result =
  result
    ^?! key "module"
      . key "name"
      . _String
      . unpacked

resultItem :: Value -> String
resultItem result =
  result
    ^?! key "item"
      . _String
      . unpacked
      . to htmlToText

resultDocs :: Value -> [String]
resultDocs result =
  result
    ^?! key "docs"
      . _String
      . unpacked
      . to htmlToText
      . to lines
      . to collapseLines
  where
    collapseLines :: [String] -> [String]
    collapseLines = \case
      [] ->
        []
      [""] ->
        []
      "" : ss ->
        case collapseLines ss of
          "" : ts -> "" : ts
          ts -> "" : ts
      s : ss ->
        s : collapseLines ss

htmlToText :: String -> String
htmlToText html =
  concat $ do
    TagText text <- parseTags html
    pure text

renderString :: Int -> Int -> String -> Termbox.Cells
renderString col row =
  foldMap (\(i, c) -> Termbox.set i row (Termbox.Cell c mempty mempty))
    . zip [col ..]

performHoogleSearch :: String -> IO [Value]
performHoogleSearch query =
  getResponseBody <$> httpJSON (fromString searchUrl)
  where
    searchUrl :: String
    searchUrl =
      "https://hoogle.haskell.org?mode=json&count=10&hoogle=" ++ query

eventAsKey :: Termbox.Event -> Maybe Termbox.Key
eventAsKey = \case
  Termbox.EventKey k -> Just k
  _ -> Nothing

keyAsChar :: Termbox.Key -> Maybe Char
keyAsChar = \case
  Termbox.KeyChar c -> Just c
  _ -> Nothing

safeTail :: [a] -> [a]
safeTail = \case
  [] -> []
  _ : xs -> xs
