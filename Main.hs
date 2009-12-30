#!/usr/bin/env runhaskell

import Prelude hiding (print)
import System.Directory
import System.FilePath
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad

import System.Environment.UTF8
import System.IO.UTF8
import System.IO (stderr, stdin, stdout)
import Language.Haskell.Exts.Annotated.ExactPrint

import HPath.Path
import HPath.Hierarchy
import qualified HPath.HaskellSrcExts as HaskellSrcExts
import qualified HPath.Cabal as Cabal




usage name                   =  unlines
 [ "USAGE:   " ++ name ++ " <Haskell identifier>"
 , ""
 , "  Print the source text corresponding to the Haskell identifier, assuming"
 , "  we are in a project directory where this source can be found."
 , ""
 ]


main                         =  do
  args                      <-  getArgs
  usage'                    <-  usage `fmap` getProgName
  case args of
    ["-h"]                  ->  out usage'
    ["-?"]                  ->  out usage'
    ["--help"]              ->  out usage'
    [arg]                   ->  case parse arg of
      Left e                ->  do
        err ("Not a path: " ++ arg ++ "\n" ++ show e)
        err usage'
      Right path            ->  do
        err (url path)
        dir                 <-  getCurrentDirectory
        (exts, roots)       <-  Cabal.info dir
        let files            =  nub [ r </> p | p <- paths path, r <- roots ]
            converted        =  nub (HaskellSrcExts.extension_conversion exts)
        (mods, errs)        <-  HaskellSrcExts.modules files converted
        let parse_errors     =  fst errs
            io_exceptions    =  snd errs
        when ((not . null) parse_errors)
             (err "Parse errors:" >> mapM_ print parse_errors)
        when ((not . null) io_exceptions)
             (err "File I/O errors:" >> mapM_ print io_exceptions)
        if null mods
          then  err "No files corresponding to this identifier." >> err usage'
          else  do
            let decls        =  HaskellSrcExts.search path (take 1 mods)
            mapM_ (out . (`exactPrint` [])) decls
    _                       ->  err usage'


err s                        =  hPutStrLn stderr s


out s                        =  hPutStrLn stdout s


