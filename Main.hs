#!/usr/bin/env runhaskell

import Prelude hiding (print)
import System.Directory
import Data.Either
import Data.Maybe

import System.Environment.UTF8
import System.IO.UTF8
import System.IO (stderr, stdin, stdout)

import HPath.Batch
import HPath.Path



usage name =
 [ "USAGE:   " ++ name ++ " <Haskell identifiers>"
 , ""
 , "  Print the source text corresponding to the Haskell identifiers."
 , ""
 ]



main                         =  do
  args                      <-  getArgs
  paths                     <-  catMaybes `fmap` mapM parse_one args
  print paths
 where
  parse_one arg              =  case parse arg of
      Right path            ->  return (Just path)
      Left e                ->  do
        err ("Not a path: " ++ arg ++ "\n" ++ show e)
        return Nothing


err s                        =  hPutStrLn stderr s

