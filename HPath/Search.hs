
module HPath.Search where

import Language.Haskell.Exts.Annotated




search qname (Module head pragmas imports declarations) = Nothing
search qname _               =  Nothing


match                       ::  QName -> Decl SrcSpanInfo -> Maybe String
match thing decl             =  case decl of
  TypeDecl _ decl _         ->  Nothing
  TypeFamDecl _ decl _      ->  Nothing
  DataDecli _ _ decl _ _    ->  Nothing
  GDataDecl _ _ decl _ _ _  ->  Nothing
  DataFamDecl _ decl _      ->  Nothing
  ClassDecl _ decl _ _      ->  Nothing
  InfixDecl _ _ op          ->  Nothing
  TypeSig name _            ->  Nothing
  FunBind clauses           ->  Nothing
  ForImp _ _ _ name _       ->  Nothing
  ForImp _ _ name _         ->  Nothing
  _                         ->  Nothing

