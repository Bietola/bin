#!/usr/bin/env stack
{- stack script
 --resolver lts-10.2
-}

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath)

parser :: Parser (FilePath, FilePath)
parser = (,) <$> argPath "src" "The source file"
             <*> argPath "dest" "The destination file"

main = do
  (src, dest) <- options "A simple `cp` script" parser
  cp src dest
