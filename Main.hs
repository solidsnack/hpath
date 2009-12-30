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



usage name                   =  unlines
 [ "USAGE:   " ++ name ++ " <Haskell identifiers>"
 , ""
 , "  Print the source text corresponding to the Haskell identifiers."
 , ""
 ]



main                         =  do
  args                      <-  getArgs
  name                      <-  getProgName
  case args of
    ["-h"]                  ->  out (usage name)
    ["-?"]                  ->  out (usage name)
    ["--help"]              ->  out (usage name)
    [arg]                   ->  case parse arg of
      Left e                ->  do
        err ("Not a path: " ++ arg ++ "\n" ++ show e)
        err (usage name)
      Right path            ->  do
        dir                 <-  getCurrentDirectory
        (_, texts):_        <-  fst `fmap` batch [path] dir
        mapM_ out texts


err s                        =  hPutStrLn stderr s


out s                        =  hPutStrLn stdout s


