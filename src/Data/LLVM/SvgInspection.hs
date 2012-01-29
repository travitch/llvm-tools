module Data.LLVM.SvgInspection ( getSvgSize ) where

import Data.ByteString ( ByteString )
import Text.XML.Light

getSvgSize :: ByteString -> Maybe (Double, Double)
getSvgSize s = do
  root <- parseXMLDoc s
  vbox <- findAttr blank_name { qName = "viewBox" } root
  let [_, _, w, h] = words vbox
  case (reads w, reads h) of
    ([(dw, "")], [(dh, "")]) -> Just (dw, dh)
    _ -> Nothing
