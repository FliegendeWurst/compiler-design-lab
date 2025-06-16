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
  -- init, body, (cond)
  -- (both init and body calculate cond, and will end with Continue/Break)
  -- (cond only provided for analysis)
  | For [Stmt] [Stmt] Expr
  -- contained in the for's body
  | ForStepLabel
  | Continue
  | Break
  | Block [Stmt]
  deriving (Show)

data Expr
  = Plain LitOrIdent
  | UnExpr UnOp LitOrIdent
  | BinExpr BinOp LitOrIdent LitOrIdent -- TODO: remove ternary1/2 from BinOp
  deriving (Show)

data LitOrIdent
  = Lit Int32
  | LitB Bool
  | Ident String
  deriving (Show)
