
module HPath.Cabal where

import Data.List
import Data.Maybe
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
  maybe (return ([], [])) (fmap e_and_s . readPackageDescription normal) cabal
 where
  find                       =  one_cabal `fmap` getDirectoryContents dir
   where
    one_cabal                =  listToMaybe . filter (isSuffixOf ".cabal")


e_and_s :: GenericPackageDescription -> ([Extension], [FilePath])
e_and_s gpkg                 =  concatP joined
 where
  joined                     =  maybeToList lib ++ exes ++ [([], ["."])]
  pkg                        =  packageDescription gpkg
  exes                       =  (build_read . buildInfo) `fmap` executables pkg
  lib                        =  (build_read . libBuildInfo) `fmap` library pkg
  build_read bi              =  (extensions bi, hsSourceDirs bi)
  concatP pairs              =  (concatMap fst pairs, concatMap snd pairs)


