module Main ( main ) where

import Control.Arrow
import Data.ByteString.Char8 ( unpack )
import Data.LLVM.VisualizeGraph

import Data.LLVM
import Data.LLVM.Analysis.CFG
import Data.LLVM.Analysis.Dominance

main :: IO ()
main = visualizeGraph optOptions mkDTs domTreeGraphvizRepr
  where
    optOptions = [ "-mem2reg", "-basicaa" ]

mkDTs :: Module -> [(String, DominatorTree)]
mkDTs m = map (getFuncName &&& toTree) fs
  where
    fs = moduleDefinedFunctions m
    toTree = dominatorTree . mkCFG
    getFuncName = unpack . identifierContent . functionName