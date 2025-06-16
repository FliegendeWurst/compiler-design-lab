module Compile.IR.X
    ( X
    , Function(..)
    , Stmt(..)
    , Op
    ) where
import qualified Compile.AST as AST
import Compile.IR.Y (Expr, LitOrIdent)
import Compile.AST (ExprType)

type X = [Function]

data Function = Function
    { name :: String
    , code :: [Stmt]
    }
    deriving (Show)

data Stmt
  = Decl ExprType String
  | Discard String
  | Asgn String Expr
  | Ret LitOrIdent
  -- condition, if, else
  | If LitOrIdent Stmt Stmt
  -- init, body
  -- (both init and body calculate cond, and will end with Continue/Break)
  | For [Stmt] [Stmt]
  | Continue
  | Break
  | Block [Stmt]
  deriving (Show)

type Op = AST.Op
