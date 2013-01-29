module Main ( main ) where

import System.Environment ( getArgs )
import LLVM.Parse

main :: IO ()
main = do
  [bcname] <- getArgs
  m <- parseLLVMFile defaultParserOptions bcname
  print m
