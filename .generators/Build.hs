#!/usr/bin/env stack
-- stack --resolver=lts-11.0 script
-- --package=unix
-- --package=shake

import Development.Shake
import Development.Shake.FilePath
import System.Posix.Env as Posix

main :: IO ()
main = do
  Just home <- Posix.getEnv "HOME"

  shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want [home </> ".dircolors"]

    home </> ".dircolors" %> \out ->
      cmd_ "./mkDirColors.hs" [out]
