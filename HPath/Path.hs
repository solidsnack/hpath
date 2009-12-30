
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
parse s = Text.ParserCombinators.Parsec.parse (qualified []) s s


qualified []                =  modules []
qualified (mod:mods)        =  do
  choice
    [ try (modules (mod:mods))
    , do
        name                <-  choice [varid, varsym, conid, consym]
        return (Path (reverse mods) mod name)
    ]


modules mods                 =  do
  mod                       <-  modid
  char '.'
  qualified (mod:mods)


url                         ::  Path -> String
url (Path h m d)             =  "hpath://" ++ intercalate "." (h ++ [m, d])


