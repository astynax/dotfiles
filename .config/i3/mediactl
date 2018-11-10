#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Turtle
import qualified Data.Text as T

type Prompt   = Text
type MenuItem = Text
type WindowID = Text
type KeyPress = Text

main :: IO ()
main = findMpv >>= maybe showMpcMenu showMpvMenu

findMpv :: IO (Maybe WindowID)
findMpv =
  toMaybe <$> procStrict "xdotool" ["search", "--name", "mpv"] empty

showMpvMenu :: WindowID -> IO ()
showMpvMenu win =
  menu "MPV:" [ "toggle"
              , "quit"
              , ".." ]
  >>= andThen
    (\case
        "toggle" -> sendToMpv win "space"
        "quit"   -> sendToMpv win "q"
        ".."     -> showMpcMenu
        other    -> die other
    )

showMpcMenu :: IO ()
showMpcMenu = do
  (_, status) <- procStrict "mpc" [] empty
  menu
    ("music: " <> head (T.lines status))
    [ "toggle"
    , "next", "prev"
    , "play", "stop"]
  >>= andThen
    (\cmd -> void $ shell ("mpc " <> cmd) empty)

menu :: Prompt -> [MenuItem] -> IO (Maybe Text)
menu prompt items =
  ((T.strip <$>) . toMaybe) <$>
  procStrict "rofi" ["-lines", "5", "-dmenu", "-p", prompt]
  (select items)

sendToMpv :: WindowID -> KeyPress -> IO ()
sendToMpv win key =
  void $ proc "xdotool" ["key", "--window", win, key] empty

-- helpers

andThen :: (a -> IO ()) -> Maybe a -> IO ()
andThen = maybe (return ())

toMaybe :: (ExitCode, Text) -> Maybe Text
toMaybe (ExitSuccess, out) = Just out
toMaybe (ExitFailure _, _) = Nothing
