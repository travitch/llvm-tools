module Main ( main ) where

import Control.Monad ( forM_ )
import Data.List ( sort )
import System.Environment ( getArgs )
import System.Process
import Text.Printf
import Text.Regex.TDFA

import LLVM.Analysis
import LLVM.Analysis.Util.Environment
import LLVM.Parse

-- Take a single bitcode file and pump it through opt -S; use
-- regex-tdfa to grep out the type specifications.  Sort them and
-- print them.
--
-- Then use llvm-data-interop to parse the bitcode and print its
-- sorted type list.

main :: IO ()
main = do
  [bcfile] <- getArgs

  -- First, get the output of opt
  optBin <- findOpt
  ll <- readProcess optBin ["-S", bcfile] ""

  let typeDefs = sort $ filter isTypeDef $ lines ll

  _ <- printf "opt types: %d\n" (length typeDefs)
  forM_ typeDefs $ \td -> putStrLn ("  " ++ td)

  Right m <- parseLLVMFile defaultParserOptions bcfile
  let ts = sort $ map show $ filter isStructType (moduleRetainedTypes m)
  _ <- printf "unified types: %d\n" (length ts)
  forM_ ts $ \td -> putStrLn ("  " ++ td)

isStructType :: Type -> Bool
isStructType t =
  case t of
    TypeStruct (Just _) _ _ -> True
    _ -> False

isTypeDef :: String -> Bool
isTypeDef s = s =~ "^%struct\\."
