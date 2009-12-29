
module HPath.HaskellSrcExts where

import Prelude hiding (readFile)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Language.Haskell.Extension as Cabal
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Extension as HaskellSrcExts

import HPath.Path
import HPath.HaskellSrcExts.Classes




search :: [Path] -> [Module SrcSpanInfo] -> Map Path [Decl SrcSpanInfo]
search paths modules         =  Map.fromList
  [ (p, concat [declarations mod q | mod <- modules])
  | p <- paths, let q = qname p ]


qname                       ::  Path -> QName SrcSpanInfo
qname (Path u m name)        =  Qual note (ModuleName note mod) name'
 where
  name'                      =  case name of
    '(':_                   ->  Symbol note name
    _                       ->  Ident note name
  mod                        =  intercalate "." (u ++ [m])
  note                       =  SrcSpanInfo (SrcSpan url 0 0 0 0) []
   where
    url                      =  "hpath://" ++ mod ++ ('.':name)


extension_conversion        ::  [Cabal.Extension] -> [HaskellSrcExts.Extension]
extension_conversion         =  impliesExts . fmap (classifyExtension . show)


