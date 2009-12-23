{-| Parser for Haskell. Not complete and just parses strings to strings. 
 -}
module HPath.Parser.Lower where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Data.Char




varid                        =  id_general small

varsym                       =  sym_general symbol
--  We aren't handling reserved operators correctly.

conid                        =  id_general large

consym                       =  sym_general colon
--  We aren't handling reserved operators correctly.

tyvar                        =  varid

tycon                        =  conid

tycls                        =  conid

modid                        =  conid


qualify p                    =  do
  mods                      <-  sepEndBy modid (char '.')
  ((,) mods) `fmap` p

sym_general p                =  do
  char '('
  c                         <-  p
  s                         <-  many (choice [symbol, colon])
  char ')'
  return ("(" ++ (c:s) ++ ")")

id_general p                 =  do
  c                         <-  p
  s                         <-  many (choice [small, large, digit, prime])
  return (c:s)


small                        =  choice [satisfy isLower, char '_']

large                        =  satisfy isUpper

symbol = choice [satisfy (`elem` "!#$%&*+./<=>?@\\^|-~"), satisfy choosy]
 where
  choosy c                   =  isSymbol c && not (elem c "_:\"'")

colon                        =  char ':'

dash                         =  char '-'

dashes                       =  dash >> dash >> many dash

prime                        =  char '\''


