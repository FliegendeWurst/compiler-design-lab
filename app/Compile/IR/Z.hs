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
  deriving (Eq, Show)

data UnOp
  = Neg
  deriving (Eq, Show)
