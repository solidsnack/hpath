
module HPath.Batch where

import System.FilePath
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.ExactPrint
import Language.Haskell.Exts.Extension

import qualified HPath.Hierarchy as Hier
import qualified HPath.HaskellSrcExts as HaskellSrcExts
import qualified HPath.Cabal as Cabal




init dir paths               =  do
  (exts, roots)             <-  Cabal.info dir
  let with_paths             =  path_map paths roots
  return ()


path_map paths roots         =  Map.fromList [ (p, all p) | p <- paths ]
 where
  all path = [ r </> p | p <- Hier.paths path, r <- roots ]


modules paths exts           =  do
  (ok, err)                 <-  (partition . zip paths) `fmap` sequence reads
  return (Map.fromList ok, err) 
 where
  reads = [ parseModuleWithMode (mode f) `fmap` readFile f | f <- paths ]
  mode f                     =  ParseMode f exts False fixities
  ParseMode _ _ _ fixities   =  defaultParseMode
  partition                  =  foldr part ([], [])
   where
    part (p, ParseOk t) (ok, err) = ((p, t):ok, err)
    part (_, ParseFailed loc s) (ok, err) = (ok, (loc, s):err)



