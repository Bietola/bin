#!/usr/bin/env stack
{- stack script
 --resolver lts-10.2
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Turtle
import BasicPrelude hiding (empty)
import qualified Data.Text as T
import Control.Exception

---------------
-- Constants --
---------------

-- The "eDP1" device always works on **richard**
defaultDisplayDevice = "eDP1"

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

------------------------------
-- XRandr utility functions --
------------------------------

extractModeInfo :: Int -> Int -> Shell [Text]
extractModeInfo resW resH =
  inproc "cvt" [tshow resW, tshow resH] "" &
      -- `Text` is easier to deal with than `Line`
      fmap lineToText &
      -- The modeline line starts with the keyword "Modeline"
      mfilter (T.isPrefixOf "Modeline") &
      -- The Modeline keyword is not needed
      fmap (tail . words)

createNewMode :: Int -> Int -> Shell Line
createNewMode resW resH = do
  modeinfo <- extractModeInfo resW resH

  -- TODO: DB
  inproc "xrandr" ("--newmode" : cleanArgs modeinfo) ""

addNewMode :: Int -> Int -> Text -> Shell Text
addNewMode resW resH displayDevice = do
  -- I don't know what the `_60.00` at the end is for, but it always seems to be appended to
  -- a new mode added with `cvt`...
  newMode <- return $ format (d%"x"%d%"_60.00") resW resH

  -- Actual connection
  inproc "xrandr"  ["--addmode", displayDevice, newMode] ""

  return newMode

setMode :: Text -> Text -> Shell Line
setMode displayDevice mode = inproc "xrandr" ["--output", displayDevice, "--mode", mode] ""

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
