module Main ( main ) where

import Control.Arrow
import Control.Applicative
import Data.GraphViz
import Data.Monoid
import Options.Applicative

import LLVM.VisualizeGraph

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.CDG
import LLVM.Analysis.CallGraph
import LLVM.Analysis.Dominance
import LLVM.Analysis.PointsTo.TrivialFunction

data Opts = Opts { outputFile :: Maybe FilePath
                 , graphType :: GraphType
                 , outputFormat :: OutputType
                 , inputFile :: FilePath
                 }

cmdOpts :: Parser Opts
cmdOpts = Opts
 <$> optional (strOption
     ( long "output"
     <> short 'o'
     <> metavar "FILE/DIR"
     <> help "The destination of a file output"))
  <*> option
      ( long "type"
      <> short 't'
      <> metavar "TYPE"
      <> help "The graph requested.  One of Cfg, Cdg, Cg, Domtree, Postdomtree")
  <*> nullOption
      ( long "format"
      <> short 'f'
      <> metavar "FORMAT"
      <> reader parseOutputType
      <> help "The type of output to produce: Gtk, Xlib, Html, Canon, XDot, Eps, Jpeg, Pdf, Png, Ps, Ps2, Svg.  Default: Gtk"
      <> value (CanvasOutput Gtk))
  <*> argument str ( metavar "FILE" )

data GraphType = Cfg
               | Cdg
               | Cg
               | Domtree
               | Postdomtree
               deriving (Read, Show, Eq, Ord)

main :: IO ()
main = execParser args >>= realMain
  where
    args = info (helper <*> cmdOpts)
      ( fullDesc
      <> progDesc "Generate the specified graph TYPE for FILE"
      <> header "ViewIRGraph - View different graphs for LLVM IR modules in a variety of formats")

realMain :: Opts -> IO ()
realMain opts = do
  let gt = graphType opts
      inFile = inputFile opts
      outFile = outputFile opts
      fmt = outputFormat opts

      vizGraph = visualizeGraph inFile outFile fmt optOptions

  case gt of
    Cfg -> vizGraph mkCFGs cfgGraphvizRepr
    Cdg -> vizGraph mkCDGs cdgGraphvizRepr
    Cg -> vizGraph mkCG cgGraphvizRepr
    Domtree -> vizGraph mkDTs domTreeGraphvizRepr
    Postdomtree -> vizGraph mkPDTs postdomTreeGraphvizRepr
  where
    optOptions = [ "-mem2reg", "-basicaa" ]

mkPDTs :: Module -> [(String, PostdominatorTree)]
mkPDTs m = map (getFuncName &&& toTree) fs
  where
    fs = moduleDefinedFunctions m
    toTree = postdominatorTree . reverseCFG . mkCFG

mkDTs :: Module -> [(String, DominatorTree)]
mkDTs m = map (getFuncName &&& toTree) fs
  where
    fs = moduleDefinedFunctions m
    toTree = dominatorTree . mkCFG

mkCG :: Module -> [(String, CallGraph)]
mkCG m = [("Module", mkCallGraph m aa [])]
  where
    aa = runPointsToAnalysis m

mkCFGs :: Module -> [(String, CFG)]
mkCFGs m = map (getFuncName &&& mkCFG) fs
  where
    fs = moduleDefinedFunctions m

mkCDGs :: Module -> [(String, CDG)]
mkCDGs m = map (getFuncName &&& toCDG) fs
  where
    fs = moduleDefinedFunctions m
    toCDG = controlDependenceGraph . mkCFG

getFuncName :: Function -> String
getFuncName = identifierAsString . functionName



-- Command line helpers


parseOutputType :: String -> Either ParseError OutputType
parseOutputType fmt =
  case fmt of
    "Html" -> Right HtmlOutput
    _ -> case reads fmt of
      [(Gtk, [])] -> Right $ CanvasOutput Gtk
      [(Xlib, [])] -> Right $ CanvasOutput Xlib
      _ -> case reads fmt of
        [(gout, [])] -> Right $ FileOutput gout
        _ -> Left $ ErrorMsg "Invalid output type"
