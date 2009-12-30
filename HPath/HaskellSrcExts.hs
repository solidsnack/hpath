
module HPath.HaskellSrcExts where

import Prelude hiding (readFile)
import System.IO.UTF8
import Data.Either
import Data.List

import Language.Haskell.Extension as Cabal
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Extension as HaskellSrcExts

import HPath.Path
import HPath.HaskellSrcExts.Classes




search :: Path -> [Module SrcSpanInfo] -> [Decl SrcSpanInfo]
search path modules          =  concatMap (`declarations` qname path) modules


qname                       ::  Path -> QName SrcSpanInfo
qname p@(Path u m name)      =  Qual note (ModuleName note mod) name'
 where
  name'                      =  case name of
    '(':_                   ->  Symbol note name
    _                       ->  Ident note name
  mod                        =  intercalate "." (u ++ [m])
  note                       =  SrcSpanInfo (SrcSpan (url p) 0 0 0 0) []


extension_conversion        ::  [Cabal.Extension] -> [HaskellSrcExts.Extension]
extension_conversion         =  impliesExts . fmap (classifyExtension . show)


modules
 :: [FilePath]
 -> [HaskellSrcExts.Extension]
 -> IO ( [Module SrcSpanInfo]
       , ([(SrcLoc, String)], [(FilePath, IOError)]))
modules paths exts           =  do
  runs                      <-  sequence (fmap carefully paths)
  let (exceptions, parses)   =  partitionEithers runs
      (ok, err)              =  partition parses
  return (ok, (err, exceptions))
 where
  carefully f                =  catch (Right `fmap` read_mod f) paired_exc
   where
    paired_exc               =  return . Left . ((,) f)
  read_mod f                 =  parseModuleWithMode (mode f) `fmap` readFile f
  mode f                     =  ParseMode f exts False True fixities
  ParseMode _ _ _ _ fixities = defaultParseMode
  partition                  =  foldr part ([], [])
   where
    part (ParseOk t) (ok, err) = (t:ok, err)
    part (ParseFailed loc s) (ok, err) = (ok, (loc, s):err)


