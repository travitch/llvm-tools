{-# LANGUAGE OverloadedStrings #-}
module Data.LLVM.HtmlWrapper ( writeHtmlIndex, writeHtmlWrapper ) where

import Control.Monad ( forM_ )
import qualified Data.ByteString.Lazy as LBS
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
    H.link ! A.href "svgpan.js" ! A.type_ "text/javascript"
  H.body $ do
    H.embed ! A.src (toValue gfilename) ! A.type_ "image/svg+xml"

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