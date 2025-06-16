module Compile.AST
  ( AST(..)
  , Stmt(..)
  , Simp(..)
  , Ctrl(..)
  , ExprType(..)
  , Expr(..)
  , HexOrDecInteger(..)
  , intValue
  , AsgnOp
  , Op(..)
  , showAsgnOp
  , posPretty
  ) where

import Data.List (intercalate)
import Text.Megaparsec

data AST =
  Function [Stmt] SourcePos

data Stmt
  = Simple Simp
  | Control Ctrl
  | Block [Stmt]

data Simp
  = Decl ExprType String SourcePos
  | Init ExprType String Expr SourcePos
  | Asgn String AsgnOp Expr SourcePos

data Ctrl
  -- Condition, if, else
  = If Expr Stmt (Maybe Stmt)
  -- Condition, Body
  | While Expr Stmt
  -- Initializer, Condition, Step, Body
  | For (Maybe Simp) Expr (Maybe Simp) Stmt
  | Continue
  | Break
  | Ret Expr SourcePos

data ExprType
  = IntT
  | BoolT
  deriving (Eq, Show)

data Expr
  = IntExpr HexOrDecInteger SourcePos
  | BoolLit Bool
  | Ident String SourcePos
  | UnExpr Op Expr
  | BinExpr Op Expr Expr

data HexOrDecInteger
  = Hex Integer
  | Dec Integer

intValue :: HexOrDecInteger -> Integer
intValue (Hex x) = x
intValue (Dec x) = x

-- Nothing means =, Just is for +=, %=, ...
type AsgnOp = Maybe Op

data Op
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
  -- Unary
  | Neg
  | LogicalNot
  | BitwiseNot
  | Nop
  deriving (Eq)

-- re-exported for convenience
posPretty :: SourcePos -> String
posPretty = sourcePosPretty

-- Some very basic pretty printing
instance Show AST where
  show (Function stmts _) =
    "Block: {\n" ++ intercalate "\n" (map show stmts) ++ "\n}"

instance Show Stmt where
  show (Simple (Decl typ name _)) = "Decl: " ++ show typ ++ " " ++ name
  show (Simple (Init typ name e _)) = "Init: " ++ show typ ++ " " ++ name ++ " = " ++ show e
  show (Simple (Asgn name op e _)) =
    "Assign: " ++ name ++ " " ++ show' op ++ " " ++ show e
    where
      show' (Just o) = show o ++ "="
      show' Nothing = "="
  show (Control (Ret e _)) = "Return: " ++ show e
  show _ = "show not implemented for this" -- TODO

instance Show Expr where
  show (IntExpr i _) = show (intValue i)
  show (BoolLit x) = show x
  show (Ident name _) = name
  show (UnExpr op e) = "(" ++ show op ++ " " ++ show e ++ ")"
  show (BinExpr op lhs rhs) =
    "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"

instance Show Op where
  show Mul = "*"
  show Add = "+"
  show Sub = "-"
  show Div = "/"
  show Mod = "%"
  show BitwiseAnd = "&"
  show BitwiseXor = "^"
  show BitwiseOr = "|"
  show LogicalAnd = "&&"
  show LogicalOr = "||"
  show LeftShift = "<<"
  show RightShift = ">>"
  show IntLt = "<"
  show IntLe = "<="
  show IntGt = ">"
  show IntGe = ">="
  show Equals = "=="
  show EqualsNot = "!="
  show Ternary1 = "?"
  show Ternary2 = ":"
  show Neg = "-"
  show LogicalNot = "!"
  show BitwiseNot = "~"
  show Nop = "[nop]"

showAsgnOp :: AsgnOp -> String
showAsgnOp (Just op) = " " ++ show op ++ "= "
showAsgnOp _ = " = "
