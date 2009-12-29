
module HPath.Batch where

import System.FilePath
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either
import Control.Monad.State

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.ExactPrint
import Language.Haskell.Exts.Extension

import HPath.Path
import qualified HPath.Hierarchy as Hier
import qualified HPath.HaskellSrcExts as HaskellSrcExts
import qualified HPath.Cabal as Cabal




init :: FilePath -> [Path] -> StateT (Map FilePath (Module SrcSpanInfo)) IO ()
init dir paths               =  do
  (exts, roots)             <-  liftIO (Cabal.info dir)
  let with_files             =  path_map paths roots
      files                  =  (Set.fromList . concat . Map.elems) with_files
      converted              =  HaskellSrcExts.extension_conversion exts
  (map, errors)             <-  liftIO (modules files (Set.fromList converted))
  put map


path_map                    ::  [Path] -> [FilePath] -> Map Path [FilePath]
path_map paths roots         =  Map.fromList [ (p, all p) | p <- paths ]
 where
  all path                   =  [ r </> p | p <- Hier.paths path, r <- roots ]


modules
 :: Set FilePath
 -> Set Extension
 -> IO ( Map FilePath (Module SrcSpanInfo)
       , ([(FilePath, (SrcLoc, String))], [(FilePath, IOError)]))
modules paths exts           =  do
  runs                      <-  sequence ((fmap carefully . Set.toList) paths)
  let (exceptions, parses)   =  partitionEithers runs
      (ok, err)              =  partition parses
  return (Map.fromList ok, (err, exceptions))
 where
  carefully f                =  catch (Right `fmap` read_mod f) paired_exc
   where
    paired_exc               =  return . Left . ((,) f)
  read_mod f                 =  do
    res                     <-  parseModuleWithMode (mode f) `fmap` readFile f
    return (f, res)
  mode f                     =  ParseMode f (Set.toList exts) False fixities
  ParseMode _ _ _ fixities   =  defaultParseMode
  partition                  =  foldr part ([], [])
   where
    part (p, ParseOk t) (ok, err) = ((p, t):ok, err)
    part (p, ParseFailed loc s) (ok, err) = (ok, (p, (loc, s)):err)



