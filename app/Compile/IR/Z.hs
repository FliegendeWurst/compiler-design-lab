module Compile.IR.Z
    ( Z
    , Function(..)
    , Stmt(..)
    , Expr(..)
    , AsgnOp
    , Op
    ) where
import qualified Compile.AST as AST

type Z = [Function]

data Function = Function
    { name :: String
    , code :: [Stmt]
    }

data Stmt 
  = Decl String
  | Init String Expr
  | Asgn String AsgnOp Expr
  | Ret Expr

data Expr
  = Lit Integer
  | Ident String
  | UnExpr Op Expr
  | BinExpr Op Expr Expr

-- Nothing means =, Just is for +=, %=, ...
type AsgnOp = AST.AsgnOp

type Op = AST.Op
