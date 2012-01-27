module Main ( main ) where

import Control.Arrow
import Data.ByteString.Char8 ( unpack )
import Data.LLVM.VisualizeGraph

import Data.LLVM
import Data.LLVM.Analysis.CFG
import Data.LLVM.Analysis.CDG

main :: IO ()
main = visualizeGraph optOptions mkCDGs cdgGraphvizRepr
  where
    optOptions = [ "-mem2reg", "-basicaa" ]

mkCDGs :: Module -> [(String, CDG)]
mkCDGs m = map (getFuncName &&& toCDG) fs
  where
    fs = moduleDefinedFunctions m
    toCDG = controlDependenceGraph . mkCFG
    getFuncName = unpack . identifierContent . functionName