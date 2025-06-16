module Compile.IR.Z
    ( Z
    , Function(..)
    , Stmt(..)
    , Simp(..)
    , Expr(..)
    , AsgnOp
    , BinOp(..)
    , UnOp(..)
    , typeExpr
    ) where
import Data.Int (Int32)
import Compile.AST (ExprType (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Util (expect)

type Z = [Function]

data Function = Function
    { name :: String
    , code :: [Stmt]
    }

data Stmt
  = Simple Simp
  -- Condition, if, else
  | If Expr Stmt Stmt
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

typeExpr :: Map String ExprType -> Expr -> ExprType
typeExpr _ (Lit _) = IntT
typeExpr _ (LitB _) = BoolT
typeExpr ctx (Ident ident) = expect ("failed to type Z-expr using ctx " ++ show ctx ++ ", identifier = " ++ ident) $ Map.lookup ident ctx
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
