module Compile.IR.Y
    ( Y
    , Function(..)
    , Stmt(..)
    , Expr(..)
    , LitOrIdent(..)
    ) where
import Data.Int (Int32)
import Compile.IR.Z (UnOp, BinOp)
import Compile.AST (ExprType)

type Y = [Function]

data Function = Function
    { name :: String
    , code :: [Stmt]
    }
    deriving (Show)

data Stmt
  = Decl ExprType String
  | Asgn String Expr
  | Ret LitOrIdent
  -- condition (0 = true, * = false), if, else
  | If LitOrIdent Stmt Stmt
  -- init, body
  -- (both init and body calculate cond, and will end with Continue/Break)
  | For [Stmt] [Stmt]
  | Continue
  | Break
  | Block [Stmt]
  deriving (Show)

data Expr
  = Plain LitOrIdent
  | UnExpr UnOp LitOrIdent
  | BinExpr BinOp LitOrIdent LitOrIdent
  deriving (Show)

data LitOrIdent
  = Lit Int32
  | LitB Bool
  | Ident String
  deriving (Show)
