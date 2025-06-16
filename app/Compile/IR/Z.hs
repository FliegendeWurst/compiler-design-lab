module Compile.IR.Z
    ( Z
    , Function(..)
    , Stmt(..)
    , Expr(..)
    , AsgnOp
    , BinOp(..)
    , UnOp(..)
    ) where
import Data.Int (Int32)

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
  = Lit Int32
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
