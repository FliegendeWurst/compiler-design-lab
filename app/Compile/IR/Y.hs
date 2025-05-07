module Compile.IR.Y
    ( Y
    , Function(..)
    , Stmt(..)
    , Expr(..)
    , LitOrIdent(..)
    , Op
    ) where
import qualified Compile.AST as AST

type Y = [Function]

data Function = Function
    { name :: String
    , code :: [Stmt]
    }
    deriving (Show)

data Stmt
  = Decl String
  | Asgn String Expr
  | Ret LitOrIdent
  deriving (Show)

data Expr
  = Plain LitOrIdent
  | UnExpr Op LitOrIdent
  | BinExpr Op LitOrIdent LitOrIdent
  deriving (Show)

data LitOrIdent
  = Lit Integer
  | Ident String
  deriving (Show)

type Op = AST.Op
