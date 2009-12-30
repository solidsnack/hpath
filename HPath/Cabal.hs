
module HPath.Cabal where

import Prelude hiding (readFile, putStrLn)
import System.IO (stderr, stdin, stdout)
import System.IO.UTF8
import System.FilePath
import Data.List
import Data.Maybe
import Control.Monad
import System.Directory

import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Language.Haskell.Extension




{-| Open a Cabal file in the given directory and tell us what extensions are
    in play and what the source directories are.
 -}
info                        ::  FilePath -> IO ([Extension], [FilePath])
info dir                     =  do
  cabal                     <-  find
  case cabal of
    Nothing                 ->  return ([], [])
    Just file               ->  do
      s                     <-  readFile file
      case parsePackageDescription s of
        ParseFailed err     ->  do
          hPutStrLn stderr ("Cabal error:" ++ "\n" ++ show err)
          return ([], [])
        ParseOk warns gpkg  ->  do
          when ((not . null) warns) $ do
            hPutStrLn stderr (unlines ("Cabal warnings:" : fmap show warns))
          return (e_and_s gpkg)
 where
  find                       =  one_cabal `fmap` getDirectoryContents dir
   where
    one_cabal                =  listToMaybe . filter (isSuffixOf ".cabal")
  e_and_s :: GenericPackageDescription -> ([Extension], [FilePath])
  e_and_s gpkg               =  concatP joined
   where
    joined                   =  maybeToList lib ++ exes ++ [([], [dir])]
    pkg                      =  packageDescription gpkg
    exes                     =  (build_read . buildInfo) `fmap` executables pkg
    lib                      =  (build_read . libBuildInfo) `fmap` library pkg
    build_read bi            =  (extensions bi, hsSourceDirs bi)
    concatP pairs            =  (concatMap fst pairs, concatMap snd pairs)


