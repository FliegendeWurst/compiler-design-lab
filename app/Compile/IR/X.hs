module Compile.IR.X
    ( X
    , Function(..)
    , Stmt(..)
    , Op
    ) where
import qualified Compile.AST as AST
import Compile.IR.Y (Expr, LitOrIdent)

type X = [Function]

data Function = Function
    { name :: String
    , code :: [Stmt]
    }
    deriving (Show)

data Stmt
  = Decl String
  | Discard String
  | Asgn String Expr
  | Ret LitOrIdent
  deriving (Show)

type Op = AST.Op
