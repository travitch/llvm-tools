{-# LANGUAGE OverloadedStrings #-}
module Data.LLVM.HtmlWrapper ( writeHtmlIndex, writeHtmlWrapper ) where

import Control.Monad ( forM_ )
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid
import System.FilePath
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 ( renderHtml )

writeHtmlWrapper :: FilePath -> FilePath -> FilePath -> String -> IO ()
writeHtmlWrapper dirname hfilename gfilename fname = do
  let wrapper = htmlWrapper fname gfilename
  LBS.writeFile (dirname </> hfilename) (renderHtml wrapper)

htmlWrapper :: String -> FilePath -> Html
htmlWrapper fname gfilename = H.docTypeHtml $ do
  H.head $ do
    H.title (toHtml fname)
    H.script ! A.src "OpenLayers.js" ! A.type_ "text/javascript" $ return ()
    H.script ! A.src "jquery-1.7.1.js" ! A.type_ "text/javascript" $ return ()
    H.script ! A.src "showGraph.js" ! A.type_ "text/javascript" $ return ()
    H.link ! A.href "graph.css" ! A.rel "stylesheet" ! A.type_ "text/css"
  H.body $ do
    H.div ! A.id "map" $ return ()
    H.script ! A.type_ "text/javascript" $ H.preEscapedString (loadScript gfilename)
  where
    loadScript n = mconcat [ "$(window).bind('load', function () {"
                           , "  showGraph('map', '", n
                           , "');\n"
                           , "});"
                           ]
writeHtmlIndex :: FilePath -> [String] -> IO ()
writeHtmlIndex dir funcNames =
  LBS.writeFile (dir </> "index.html") (renderHtml (htmlIndex funcNames))

htmlIndex :: [String] -> Html
htmlIndex funcNames = H.docTypeHtml $ do
  H.head $ do
    H.title "Module Summary"
  H.body $ do
    H.ul $ forM_ funcNames mkLink
  where
    mkLink n = H.li $ do
      let ref = "graphs" </> n <.> "html"
      H.a ! A.href (toValue ref) $ toHtml n