
module HPath.Path
  ( Path(..)
  , parse
  ) where

import qualified Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Char

import HPath.Parser.Lower




data Path                    =  Path [String] String String
deriving instance Show Path


parse                       ::  String -> Either ParseError Path
parse s                      =  Text.ParserCombinators.Parsec.parse q s s


q                           ::  CharParser st Path
q                            =  do
  modules                   <-  sepEndBy1 modid (char '.')
  name                      <-  choice [varid, varsym, conid, consym]
  return (Path (tail modules) (head modules) name)



