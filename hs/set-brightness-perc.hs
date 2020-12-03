#!/usr/bin/env stack
{- stack script
 --resolver lts-10.2
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Turtle
import BasicPrelude hiding (empty, FilePath)
import qualified Data.Text as T
import Control.Exception

---------------
-- Constants --
---------------

brightnessFile :: FilePath
brightnessFile = fromText "/sys/class/backlight/intel_backlight/brightness"

---------------------
-- Argument Parser --
---------------------

argParser = options "Simple script to add and easily set xrandr resolutions" parser
  where parser = (,) <$> argInt "reswidth" "The resolution width"
                     <*> argInt "resheight" "The resolution height"

-- This function should make a Haskell list behave like a bash list of arguments when passed to a bash command
-- using `proc` or `inproc`.
--
-- NB. For now, the only thing that happens is that external \" are unescaped, since they would just disappear
-- when passed to a bash command
--     e.g. `echo "hello"` vs `echo \"hello\"`
cleanArgs :: [Text] -> [Text]
cleanArgs = map cleanArg
  where unescapeQuote quote = if quote == "\"" then "" else quote
        unquote arg = (T.take 1 arg, T.take (T.length arg - 2) $ T.drop 1 arg, T.drop (T.length arg - 1) arg)
        cleanArg arg = let (openQuote, body, closeQuote) = unquote arg
                        in unescapeQuote openQuote ++ body ++ unescapeQuote closeQuote

-----------------------
-- utility functions --
-----------------------

setBrightness amount = output brightnessFile amount

----------------------
-- User interaction --
----------------------

interactiveAddMode :: Int -> Int -> Shell ()
interactiveAddMode resW resH = do
  -- New mode is created
  createNewMode resW resH

  -- ...and added to the list of available modes
  newMode <- addNewMode resW resH defaultDisplayDevice

  echo "Mode added successfully, set it now? (p to preview) [y/N/p]"
  res <- readline
  let setModeOnDef = setMode defaultDisplayDevice
    in case res of
      -- Permanent switch
      Just "y" -> do
        echo "Applying mode permanently."
        view $ setModeOnDef newMode

      -- Preview
      Just "p" -> do
        -- TODO: Find a way to retrieve the current mode
        oldMode <- "1920x1080"
        setModeOnDef newMode
        echo "Preview started. New mode active for 5 seconds."
        sleep 5
        view $ setModeOnDef oldMode

      -- No mode change
      _ -> echo "Ok then."

----------
-- Main --
----------

main = do
  -- Arguments are resolution width and height of new mode
  (resW, resH) <- argParser

  -- Main program
  (sh $ interactiveAddMode resW resH)
    -- If any internally used program fails, this message is going to be issued
    `catch` \(e :: ExitCode) ->
      error $ "Stopping add-xrandr-resolution because of previous process error: " ++ displayException e
