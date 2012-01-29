module Data.LLVM.VisualizeGraph ( visualizeGraph ) where

import GHC.Conc ( getNumCapabilities )

import Control.Concurrent.ParallelIO.Local
import Control.Monad ( when )
import qualified Data.ByteString as BS
import Data.GraphViz
import Data.Maybe ( isNothing )

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import System.Directory
import System.Exit
import System.FilePath

import Paths_llvm_tools

import Data.LLVM
import Data.LLVM.Testing ( buildModule )
import Data.LLVM.Parse ( defaultParserOptions, parseLLVMFile )

import Data.LLVM.HtmlWrapper
import Data.LLVM.SvgInspection

data Opts = Opts { inputFile :: Maybe FilePath
                 , outputFile :: Maybe FilePath
                 , outputFormat :: OutputType
                 , wantsHelp :: Bool
                 }

data OutputType = CanvasOutput GraphvizCanvas
                | FileOutput GraphvizOutput
                | HtmlOutput
                deriving (Show)

cmdOpts :: Opts -> Mode Opts
cmdOpts defs = mode "VisualizeGraph" defs desc infileArg as
  where
    infileArg = flagArg setInput "INPUT"
    desc = "A generic graph viewing frontend"
    as = [ flagReq ["output", "o"] setOutput "[FILE or DIR]" "The destination of a file output"
         , flagReq ["format", "f"] setFormat "GVOUT" "The type of output to produce: Gtk, Xlib, XDot, Eps, Jpeg, Pdf, Png, Ps, Ps2, Svg.  Default: Gtk"
         , flagHelpSimple setHelp
         ]

defaultOptions :: Opts
defaultOptions = Opts { inputFile = Nothing
                      , outputFile = Nothing
                      , outputFormat = CanvasOutput Gtk
                      , wantsHelp = False
                      }

showHelpAndExit :: Mode a -> IO b -> IO b
showHelpAndExit args exitCmd = do
  putStrLn $ showText (Wrap 80) $ helpText [] HelpFormatOne args
  exitCmd

-- | Visualize a graph-based analysis with graphviz.  It handles many
-- common options including both file and canvas output.
visualizeGraph :: (PrintDotRepr dg n)
                  => [String] -- ^ Module optimization flags
                  -> (Module -> [(String, a)]) -- ^ A function to turn a Module into some graphs
                  -> (a -> dg n) -- ^ A function to turn each graph into a GraphViz DotGraph
                  -> IO ()
visualizeGraph optOptions fromModule toGraph  = do
  let arguments = cmdOpts defaultOptions
  opts <- processArgs arguments

  when (wantsHelp opts) (showHelpAndExit arguments exitSuccess)
  when (isNothing (inputFile opts)) $ do
    putStrLn "Input file missing"
    exitFailure

  let Just infile = inputFile opts

  let p = parseLLVMFile defaultParserOptions
  m <- buildModule optOptions p infile
  let gs = fromModule m

  case outputFormat opts of
    HtmlOutput -> do
      when (isNothing (outputFile opts)) $ do
        putStrLn "Output directory not specified"
        exitFailure
      -- Make a directory for all of the output and render each graph
      -- with graphviz to svg format.  For each svg, create an html
      -- wrapper page (with an index page).  The html page should be simple
      -- and just embed the SVG and load svgpan (and maybe jquery)
      let Just outFile = outputFile opts
          gdir = outFile </> "graphs"

      createDirectoryIfMissing True gdir
      installStaticFile gdir "OpenLayers.js"
      installStaticFile gdir "jquery-1.7.1.js"
      installStaticFile gdir "showGraph.js"
      installStaticFile gdir "graph.css"

      caps <- getNumCapabilities
      let actions = map (makeFunctionPage toGraph gdir) gs
      withPool caps $ \capPool -> parallel_ capPool actions
      writeHtmlIndex outFile (map fst gs)

    -- If we are showing canvases, ignore function names
    CanvasOutput o -> mapM_ (\(_,g) -> runGraphvizCanvas' (toGraph g) o) gs

    FileOutput o -> do
      when (isNothing (outputFile opts)) $ do
        putStrLn "Output file not specified"
        exitFailure
      let Just outFile = outputFile opts
      case gs of
        [(_, g)] -> runGraphviz (toGraph g) o outFile >> return ()
        _ -> do
          -- If we have more than one function, put all of them in
          -- the given directory
          createDirectoryIfMissing True outFile
          mapM_ (writeDotGraph toGraph outFile o) gs

installStaticFile :: FilePath -> FilePath -> IO ()
installStaticFile dir name = do
  file <- getDataFileName ("share" </> name)
  copyFile file (dir </> name)

makeFunctionPage :: (PrintDotRepr dg n)
                    => (a -> dg n)
                    -> FilePath
                    -> (FilePath, a)
                    -> IO ()
makeFunctionPage toGraph gdir (fname, g) = do
  let svgname = gdir </> gfilename
      dg = toGraph g
  -- Use the more general graphvizWithHandle so that we can read the
  -- generated image before writing it to disk.  This lets us extract
  -- its dimensions.  The other alternative is to just use the default
  -- graphviz to create a file and then read the file - this led to
  -- races when writing with multiple threads.  This approach is
  -- safer.
  dims <- graphvizWithHandle (commandFor dg) dg Svg $ \h -> do
    svgContent <- BS.hGetContents h
    BS.writeFile svgname svgContent
    return (getSvgSize svgContent)
  let Just (w, h) = dims
  writeHtmlWrapper gdir hfilename gfilename fname w h
  where
    gfilename = fname <.> "svg"
    hfilename = fname <.> "html"

writeDotGraph :: (PrintDotRepr dg n)
                 => (a -> dg n)
                 -> FilePath
                 -> GraphvizOutput
                 -> (FilePath, a)
                 -> IO ()
writeDotGraph toGraph dirname o (funcName, g) =
  runGraphviz (toGraph g) o filename >> return ()
  where
    filename = dirname </> funcName <.> toExt o

toExt :: GraphvizOutput -> String
toExt o =
  case o of
    XDot -> "dot"
    Eps -> "eps"
    Fig -> "fig"
    Jpeg -> "jpg"
    Pdf -> "pdf"
    Png -> "png"
    Ps -> "ps"
    Ps2 -> "ps"
    Svg -> "svg"
    _ -> error $ "Unsupported format: " ++ show o

-- Command line helpers

setHelp :: Opts -> Opts
setHelp opts = opts { wantsHelp = True }

setInput :: String -> Opts -> Either String Opts
setInput inf opts@Opts { inputFile = Nothing } =
  Right opts { inputFile = Just inf }
setInput _ _ = Left "Only one input file is allowed"

setOutput :: String -> Opts -> Either String Opts
setOutput outf opts@Opts { outputFile = Nothing } =
  Right opts { outputFile = Just outf }
setOutput _ _ = Left "Only one output file is allowed"

setFormat :: String -> Opts -> Either String Opts
setFormat fmt opts =
  case fmt of
    "Html" -> Right opts { outputFormat = HtmlOutput }
    _ -> case reads fmt of
      [(Gtk, [])] -> Right opts { outputFormat = CanvasOutput Gtk }
      [(Xlib, [])] -> Right opts { outputFormat = CanvasOutput Xlib }
      _ -> case reads fmt of
        [(gout, [])] -> Right opts { outputFormat = FileOutput gout }
        _ -> Left ("Unrecognized output format: " ++ fmt)