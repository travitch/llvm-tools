module LLVM.VisualizeGraph (
  OutputType(..),
  visualizeGraph
  ) where

import GHC.Conc ( getNumCapabilities )

import Control.Arrow
import Control.Concurrent.ParallelIO.Local
import Control.Monad ( when )
import qualified Data.ByteString as BS
import Data.GraphViz
import Data.Maybe ( isNothing )

import System.Directory
import System.FilePath
import System.FilePath.Glob

import Paths_llvm_tools

import LLVM.Analysis
import LLVM.Analysis.Util.Testing ( buildModule )
import LLVM.Parse ( defaultParserOptions, parseLLVMFile )

import LLVM.HtmlWrapper
import LLVM.SvgInspection

data OutputType = CanvasOutput GraphvizCanvas
                | FileOutput GraphvizOutput
                | HtmlOutput
                deriving (Show)


-- | Visualize a graph-based analysis with graphviz.  It handles many
-- common options including both file and canvas output.
visualizeGraph :: (PrintDotRepr dg n)
                  => FilePath -- ^ Input file name
                  -> Maybe FilePath -- ^ Output file name
                  -> OutputType -- ^ Type of output requested
                  -> [String] -- ^ Module optimization flags
                  -> (Module -> [(String, a)]) -- ^ A function to turn a Module into some graphs
                  -> (a -> dg n) -- ^ A function to turn each graph into a GraphViz DotGraph
                  -> IO ()
visualizeGraph inFile outFile fmt optOptions fromModule toGraph  = do
  let p = parseLLVMFile defaultParserOptions
  m <- buildModule optOptions p inFile
  let gs = fromModule m

  case fmt of
    HtmlOutput -> do
      when (isNothing outFile) $ ioError $ userError "Output directory not specified"
      -- Make a directory for all of the output and render each graph
      -- with graphviz to svg format.  For each svg, create an html
      -- wrapper page (with an index page).  The html page should be simple
      -- and just embed the SVG and load svgpan (and maybe jquery)
      let Just outFile' = outFile
          gdir = outFile' </> "graphs"

      createDirectoryIfMissing True gdir
      let jsAndCss = [ "OpenLayers.js"
                     , "jquery-1.7.1.js"
                     , "showGraph.js"
                     , "graph.css"
                     ]
      mapM_ (installStaticFile gdir) jsAndCss
      installStaticSubdir "img" gdir

      caps <- getNumCapabilities
      let actions = map (makeFunctionPage toGraph gdir) gs
      withPool caps $ \capPool -> parallel_ capPool actions
      writeHtmlIndex outFile' (map fst gs)

    -- If we are showing canvases, ignore function names
    CanvasOutput o -> mapM_ (\(_,g) -> runGraphvizCanvas' (toGraph g) o) gs

    FileOutput o -> do
      when (isNothing outFile) $ ioError $ userError "Output file not specified"
      let Just outFile' = outFile
      case gs of
        [(_, g)] -> runGraphviz (toGraph g) o outFile' >> return ()
        _ -> do
          -- If we have more than one function, put all of them in
          -- the given directory
          createDirectoryIfMissing True outFile'
          mapM_ (writeDotGraph toGraph outFile' o) gs

installStaticFile :: FilePath -> FilePath -> IO ()
installStaticFile dir name = do
  file <- getDataFileName ("share" </> name)
  copyFile file (dir </> name)

-- | Install all of the files in the given subdir of the datadir to a
-- destdir.  The subdir is created (this is basically cp -R).
installStaticSubdir :: FilePath -> FilePath -> IO ()
installStaticSubdir sdir destdir = do
  dd <- getDataDir
  let patt = dd </> "share" </> sdir </> "*"
  files <- namesMatching patt
  let toDest f = destdir </> sdir </> takeFileName f
  let namesAndDests = map (id &&& toDest) files
  createDirectoryIfMissing True (destdir </> sdir)
  mapM_ (uncurry copyFile) namesAndDests

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
    Canon -> "dot"
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
