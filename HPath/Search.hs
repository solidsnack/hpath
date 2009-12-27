
module HPath.Search where

import Language.Haskell.Exts.Annotated
import Data.List




class (Annotated ast) => SearchModule ast where
  declarations              ::  Module t -> ast t -> [Decl t]

instance SearchModule Name where
  declarations mod name      =  case mod of
    Module _ _ _ _ decls    ->  filter' decls
    XmlHybrid _ _ _ _ decls _ _ _ _ -> filter' decls
    _                       ->  []
   where
    filter'                  =  filter (`match` name)

instance SearchModule QName where
  declarations m qname =
    case qname of
      UnQual _ name         ->  declarations m name
      Qual _ mname name     ->  if module_name mname == module_name m
                                  then  declarations m name
                                  else  []
      _                     ->  []

instance SearchModule Op where
  declarations m (VarOp _ name) = declarations m name
  declarations m (ConOp _ name) = declarations m name

instance SearchModule QOp where
  declarations m (QVarOp _ qname) = declarations m qname
  declarations m (QConOp _ qname) = declarations m qname


class (Annotated ast) => HasModuleName ast where
  module_name               ::  ast t -> String

instance HasModuleName ModuleName where
  module_name (ModuleName _ s) = s

instance HasModuleName ModuleHead where
  module_name (ModuleHead _ name _ _) = module_name name 

instance HasModuleName Module where
  module_name (Module _ (Just head) _ _ _) = module_name head
  module_name (XmlPage _ (ModuleName _ s) _ _ _ _ _) = s
  module_name (XmlHybrid _ (Just head) _ _ _ _ _ _ _) = module_name head
  module_name _              =  ""

instance HasModuleName QName where
  module_name (Qual _ name _) = module_name name
  module_name _              =  ""

instance HasModuleName QOp where
  module_name (QVarOp _ qname) = module_name qname
  module_name (QConOp _ qname) = module_name qname


class (Annotated ast) => MentionsNames ast where
  match                     ::  ast t -> Name t -> Bool

instance MentionsNames Decl where 
  match decl name            =  case decl of
    TypeDecl _ d _          ->  match d name
    TypeFamDecl _ d _       ->  match d name
    DataDecl _ _ _ d _ _    ->  match d name
    GDataDecl _ _ _ d _ _ _ ->  match d name
    DataFamDecl _ _ d _     ->  match d name
    ClassDecl _ _ d _ _     ->  match d name
    InfixDecl _ _ _ ops     ->  any (`match` name) ops
    TypeSig _ names _       ->  any (`match` name) names
    FunBind _ clauses       ->  any (`match` name) clauses
    ForImp _ _ _ _ name' _  ->  match name name'
    ForExp _ _ _ name' _    ->  match name name'
    _                       ->  False

instance MentionsNames DeclHead where
  match head name            =  case head of
    DHead _ name' _         ->  match name name'
    DHInfix _ _ name' _     ->  match name name'
    DHParen _ head'         ->  match head' name

instance MentionsNames Op where
  match op name              =  case op of
    VarOp _ name'           ->  match name name'
    ConOp _ name'           ->  match name name'

instance MentionsNames Match where
  match m name               =  case m of
    Match _ name' _ _ _     ->  match name name'
    InfixMatch _ _ n _ _ _  ->  match name n

instance MentionsNames Name where
  match (Ident _ s) (Ident _ s') = s == s'
  match (Symbol _ s) (Symbol _ s') = s == s'
  match _ _                  =  False

