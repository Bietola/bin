#!/usr/bin/env stack
{- stack script
 --optimize
 --resolver lts-14.2
 --package turtle
-}

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath)

parser :: Parser (FilePath, FilePath)
parser = (,) <$> argPath "SOURCE_PATH" "Symlink is created to this file"
             <*> argPath  "SYMLINK_PATH"  "Symlink is created at this path"

main = do
  (sourcePath, linkPath) <- options
    "Create symlink while creating the whole supporting tree"
    parser

  mktree $ parent linkPath
  wd <- pwd
  symlink (wd </> sourcePath) linkPath
