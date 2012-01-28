{-# LANGUAGE OverloadedStrings #-}
module Data.LLVM.HtmlWrapper ( writeHtmlIndex, writeHtmlWrapper ) where

import qualified Data.ByteString.Lazy as LBS
import System.FilePath
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 ( renderHtml )

writeHtmlWrapper :: FilePath -> FilePath -> FilePath -> String -> IO ()
writeHtmlWrapper dirname hfilename gfilename fname = do
  let wrapper = htmlWrapper fname gfilename
  LBS.writeFile (dirname </> hfilename) (renderHtml wrapper)

htmlWrapper :: String -> FilePath -> Html
htmlWrapper fname gfilename = docTypeHtml $ do
  H.head $ do
    H.title (toHtml fname)
    H.link ! A.href "svgpan.js" ! A.type_ "text/javascript"
  H.body $ do
    H.embed ! A.src (toValue gfilename) ! A.type_ "image/svg+xml"

writeHtmlIndex dir funcnames = undefined