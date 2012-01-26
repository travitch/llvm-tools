module Main ( main ) where

import Control.Arrow
import Data.ByteString.Char8 ( unpack )
import Data.LLVM.VisualizeGraph

import Data.LLVM
import Data.LLVM.CFG
import Data.LLVM.Analysis.Dominance

main :: IO ()
main = visualizeGraph optOptions mkPDTs postdomTreeGraphvizRepr
  where
    optOptions = [ "-mem2reg", "-basicaa" ]

mkPDTs :: Module -> [(String, PostdominatorTree)]
mkPDTs m = map (getFuncName &&& toTree) fs
  where
    fs = moduleDefinedFunctions m
    toTree = postdominatorTree . reverseCFG . mkCFG
    getFuncName = unpack . identifierContent . functionName