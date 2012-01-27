module Main ( main ) where

import Data.LLVM.VisualizeGraph

import Data.LLVM
import Data.LLVM.Analysis.CallGraph
import Data.LLVM.Analysis.PointsTo.TrivialFunction

main :: IO ()
main = visualizeGraph optOptions mkCG cgGraphvizRepr
  where
    optOptions = [ "-mem2reg", "-basicaa" ]

mkCG :: Module -> [(String, CallGraph)]
mkCG m = [("Module", mkCallGraph m aa [])]
  where
    aa = runPointsToAnalysis m
