
module HPath.HaskellSrcExts where

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


