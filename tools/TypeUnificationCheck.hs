-- | Take a single bitcode file and pump it through opt -S and parse
-- out the named type definitions (structs, unions, and classes).
-- Sort them and print them.
--
-- Then use llvm-data-interop to parse the bitcode and print its
-- sorted type list.  It will be apparent if types were properly
-- unified or not.
module Main ( main ) where

import Control.Monad ( forM_ )
import Data.Attoparsec.ByteString.Char8
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Process as P
import Data.List ( sort )
import Data.Monoid
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import System.Environment ( getArgs )
import Text.Printf

import LLVM.Analysis
-- import LLVM.Analysis.Util.Environment
import LLVM.Parse

llvmTypesParser :: Parser [ByteString]
llvmTypesParser = do
  skipWhile (/='%')
  res <- many1 typeLineParser
  skipWhile (const True)
  return res

typePrefix :: Parser ByteString
typePrefix = choice [ string (BS.pack "%struct")
                    , string (BS.pack "%union")
                    , string (BS.pack "%class")
                    ]

typeLineParser :: Parser ByteString
typeLineParser = do
  pfx <- typePrefix
  suffix <- takeTill (=='\n')
  endOfLine
  return $ pfx `mappend` suffix

main :: IO ()
main = do
  [bcfile] <- getArgs

  -- First, get the output of opt.  This uses process-conduit and
  -- attoparsec-conduit to parse the output of opt in constant space.
  typeLines <- runResourceT $ do
    sourceProcess (P.proc "opt" ["-S", bcfile]) $$ sinkParser llvmTypesParser

  let typeDefs = sort typeLines

  _ <- printf "opt types: %d\n" (length typeDefs)
  forM_ typeDefs $ \td -> BS.putStrLn (BS.pack "  " `mappend` td)

  m <- parseLLVMFile defaultParserOptions bcfile
  let ts = sort $ map show $ filter isStructType (moduleRetainedTypes m)
  _ <- printf "unified types: %d\n" (length ts)
  forM_ ts $ \td -> putStrLn ("  " ++ td)

isStructType :: Type -> Bool
isStructType t =
  case t of
    TypeStruct (Right _) _ _ -> True
    _ -> False
