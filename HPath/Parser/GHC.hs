
module HPath.Parser.GHC where

import Parser
import Lexer
import StringBuffer
import FastString
import SrcLoc
import DynFlags

import qualified System.IO.UTF8 as UTF8




tokenize_file path           =  do
  source                    <-  UTF8.readFile path
  buffy                     <-  stringToStringBuffer s
  d                         <-  initDynFlags defaultDynFlags
  return (lexTokenStream buffy loc d)
 where
  loc                        =  mkSrcLoc (mkFastString path) 1 1


display_lexer_result        ::  ParseResult [Located Token] -> String

display_SrcLoc loc           =  unwords
  [ unpackFS (srcLocFile loc), show (srcLocLine loc), show (srcLocCol loc) ]

display_SrcSpan span         =  unwords
  [ unpackFS (srcSpanFile span)
  , display (srcSpanStartLine span) (srcSpanStartCol span)
  , display (srcSpanEndLine span) (srcSpanEndCol span)
  ]
 where
  display line col = "(Line " ++ show line ++ ", Column " ++ show col ++ ")"


