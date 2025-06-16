module Compile.IR.Z
    ( Z
    , Function(..)
    , Stmt(..)
    , Simp(..)
    , Expr(..)
    , AsgnOp
    , BinOp(..)
    , UnOp(..)
    ) where
import Data.Int (Int32)
import Compile.AST (ExprType)

type Z = [Function]

data Function = Function
    { name :: String
    , code :: [Stmt]
    }

data Stmt
  = Simple Simp
  -- Condition, if, else
  | If Expr Stmt (Maybe Stmt)
  -- Condition, Body
  | While Expr Stmt
  -- Initializer, Condition, Step, Body
  | For (Maybe Simp) Expr (Maybe Simp) Stmt
  | Continue
  | Break
  | Ret Expr
  | Block [Stmt]

data Simp
  = Decl ExprType String
  | Init ExprType String Expr
  | Asgn String AsgnOp Expr

data Expr
  = Lit Int32
  | LitB Bool
  | Ident String
  | UnExpr UnOp Expr
  | BinExpr BinOp Expr Expr

-- Nothing means =, Just is for +=, %=, ...
type AsgnOp = Maybe BinOp

data BinOp
  = Mul
  | Add
  | Sub
  | Div
  | Mod
  | BitwiseAnd
  | BitwiseXor
  | BitwiseOr
  | LogicalAnd
  | LogicalOr
  | LeftShift
  | RightShift
  | IntLt
  | IntLe
  | IntGt
  | IntGe
  | Equals
  | EqualsNot
  | Ternary1
  | Ternary2
  deriving (Eq, Show)

data UnOp
  = Neg
  | LogicalNot
  | BitwiseNot
  deriving (Eq, Show)
