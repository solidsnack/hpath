
module HPath.Path
  ( Path(..)
  , parse
  , url
  ) where

import Data.List

import qualified Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Char

import HPath.Parser.Lower




data Path                    =  Path [String] String String
deriving instance Eq Path
deriving instance Ord Path
deriving instance Show Path


parse                       ::  String -> Either ParseError Path
parse s                      =  Text.ParserCombinators.Parsec.parse q s s


q                           ::  CharParser st Path
q                            =  do
  modules                   <-  sepEndBy1 modid (char '.')
  name                      <-  choice [varid, varsym, conid, consym]
  return (Path (tail modules) (head modules) name)


url                         ::  Path -> String
url (Path h m d)             =  "hpath://" ++ intercalate "." (h ++ [m, d])


