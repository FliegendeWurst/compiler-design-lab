module Compile.IR.Y
    ( Y
    , Function(..)
    , Stmt(..)
    , Expr(..)
    , LitOrIdent(..)
    ) where
import Data.Int (Int32)
import Compile.IR.Z (UnOp, BinOp)

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
  | UnExpr UnOp LitOrIdent
  | BinExpr BinOp LitOrIdent LitOrIdent
  deriving (Show)

data LitOrIdent
  = Lit Int32
  | Ident String
  deriving (Show)
