module Main ( main ) where

import System.Environment ( getArgs )
import LLVM.Parse

main :: IO ()
main = do
  [bcname] <- getArgs
  Right m <- parseLLVMFile defaultParserOptions bcname
  print m
