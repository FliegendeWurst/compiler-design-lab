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
  , opIsIntIntToInt
  , opIsIntIntToBool
  , opIsBoolBoolToBool
  , typeExpr
  ) where

import Data.List (intercalate)
import Text.Megaparsec
import Prelude hiding (init)
import Data.Map (Map)
import qualified Data.Map as Map
import Util (unwrap)

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
  deriving (Show)

data Ctrl
  -- Condition, if, else
  = If Expr Stmt Stmt
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

typeExpr :: Map String ExprType -> Expr -> ExprType
typeExpr _ (IntExpr _ _) = IntT
typeExpr _ (BoolLit _) = BoolT
typeExpr ctx (Ident ident _) = unwrap $ Map.lookup ident ctx
typeExpr ctx (UnExpr _ e) = typeExpr ctx e
typeExpr _ (BinExpr LogicalAnd _ _) = BoolT
typeExpr _ (BinExpr LogicalOr _ _) = BoolT
typeExpr _ (BinExpr IntLt _ _) = BoolT
typeExpr _ (BinExpr IntLe _ _) = BoolT
typeExpr _ (BinExpr IntGt _ _) = BoolT
typeExpr _ (BinExpr IntGe _ _) = BoolT
typeExpr _ (BinExpr Equals _ _) = BoolT
typeExpr _ (BinExpr EqualsNot _ _) = BoolT
typeExpr _ctx (BinExpr Ternary1 _ _) = error "ternary type" -- FIXME
typeExpr _ctx (BinExpr Ternary2 _ _) = error "ternary type" -- FIXME
typeExpr _ (BinExpr {}) = IntT

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

opIsIntIntToInt :: Op -> Bool
opIsIntIntToInt Mul = True
opIsIntIntToInt Add = True
opIsIntIntToInt Sub = True
opIsIntIntToInt Div = True
opIsIntIntToInt Mod = True
opIsIntIntToInt BitwiseAnd = True
opIsIntIntToInt BitwiseXor = True
opIsIntIntToInt BitwiseOr = True
opIsIntIntToInt LeftShift = True
opIsIntIntToInt RightShift = True
opIsIntIntToInt _ = False

opIsIntIntToBool :: Op -> Bool
opIsIntIntToBool IntLt = True
opIsIntIntToBool IntLe = True
opIsIntIntToBool IntGt = True
opIsIntIntToBool IntGe = True
opIsIntIntToBool Equals = True
opIsIntIntToBool EqualsNot = True
opIsIntIntToBool _ = False

opIsBoolBoolToBool :: Op -> Bool
opIsBoolBoolToBool LogicalAnd = True
opIsBoolBoolToBool LogicalOr = True
opIsBoolBoolToBool Equals = True
opIsBoolBoolToBool EqualsNot = True
opIsBoolBoolToBool _ = False

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
  show (Block xs) = "Block: {\n" ++ show xs ++ "\n}"
  show (Control (Continue)) = "Continue"
  show (Control (Break)) = "Break"
  show (Control (If cond ifB elseB)) = "If: " ++ show cond ++ "{\n" ++ show ifB ++ "\n} else {\n" ++ show elseB ++ "\n}"
  show (Control (For init cond step body)) = "For: " ++ show init ++ "; " ++ show cond ++ "; " ++ show step ++ "{\n" ++ show body ++ "\n}"
  show (Control (While cond body)) = "While: " ++ show cond ++ "{\n" ++ show body ++ "\n}"

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
