#!/usr/bin/env stack
-- stack --resolver=lts-13.24 runghc --package relude

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Relude
import System.Environment (getArgs)

data Code = Code
  { at :: Last Text
  , fg :: Last Text
  , bg :: Last Text }

instance Semigroup Code where
  (Code a1 f1 b1) <> (Code a2 f2 b2) = Code (a1 <> a2) (f1 <> f2) (b1 <> b2)

instance Monoid Code where
  mempty = Code mempty mempty mempty
  mappend = (<>)

fromCode :: Code -> Text
fromCode code = mconcat . intersperse ";" $ un at <> un fg <> un bg
  where
    un field = maybeToList . getLast $ field code

aNone, aBold, aUnderscore, aBlink, aReverse, aConcealed :: Code
[aNone, aBold, aUnderscore, aBlink, aReverse, aConcealed] =
  map (\a -> mempty { at = Last (Just a)})
  ["00", "01", "04", "05", "07", "08"]

fBlack, fRed, fGreen, fYellow, fBlue, fMagenta, fCyan, fWhite :: Code
[fBlack, fRed, fGreen, fYellow, fBlue, fMagenta, fCyan, fWhite] =
  map (\a -> mempty { fg = Last (Just a)})
  ["30", "31", "32", "33", "34", "35", "36", "37"]

bBlack, bRed, bGreen, bYellow, bBlue, bMagenta, bCyan, bWhite :: Code
[bBlack, bRed, bGreen, bYellow, bBlue, bMagenta, bCyan, bWhite] =
  map (\a -> mempty { fg = Last (Just a)})
  ["40", "41", "42", "43", "44", "45", "46", "47"]

dircolors :: Text
dircolors = unlines $ concat
  [ h "Special"
  , "EXEC" ~~ aBold <> fGreen
  , "DIR" ~~ aBold <> fBlue
--  , ["LINK\ttarget"]

  , h "Archives"
  , [".zip", ".tgz", ".deb", ".z", ".tar", ".gz", ".rar", ".jar", ".7z"]
    ..~ aBold <> fMagenta

  , h "Images"
  , [".png", ".jpg", ".jpeg", ".gif", ".svg", ".tga"]
    ..~ fYellow

  , h "Audio"
  , [".mp3", ".wav", ".ogg", ".flac"]
    ..~ aBold <> fRed

  , h "Video"
  , [".mp4", ".flv", ".avi", ".mkv", ".apng", ".ogv", ".webm"]
    ..~ aBold <> fYellow

  , h "Documents"
  , [ ".txt", ".md", ".org", ".rst", ".pdf", ".odt", ".cbr", ".cbz"
    , ".mobi", ".epub", ".fb2", ".srt", ".ass" ]
    ..~ aBold <> fCyan

  , h "Sources"
  , [ ".py", ".hs", ".lhs", ".elm", ".purs", ".sh", ".bash", ".rkt"
    , ".el", ".scm", ".rs", ".json", ".toml", ".yaml", ".yml", ".rc"
    , ".kt", ".kts" ]
    ..~ fCyan
  ]
  where
    h t = ["# " <> t]
    infixl 2 ~~
    infixl 2 ..~
    t ~~ c = [t <> "\t" <> fromCode c]
    ts ..~ c = concatMap (~~ c) ts

main :: IO ()
main = getArgs >>= \case
  ["-"] -> putTextLn dircolors
  [fn]  -> writeFileText fn dircolors
  _     -> putTextLn "usage: script (-|filename)"
