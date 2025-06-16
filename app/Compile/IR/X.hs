{-# LANGUAGE InstanceSigs #-}
module Compile.IR.X
    ( X
    , Function(..)
    , Stmt(..)
    , Op
    ) where
import qualified Compile.AST as AST
import Compile.IR.Y (Expr, LitOrIdent)
import Compile.AST (ExprType)
import Prelude hiding (init)

type X = [Function]

data Function = Function
    { name :: String
    , code :: [Stmt]
    }

data Stmt
  = Decl ExprType String
  | Discard String
  | Asgn String Expr
  | Ret LitOrIdent
  -- condition (0 = true, * = false), if, else
  | If LitOrIdent Stmt Stmt
  -- init, body
  -- (both init and body calculate cond, and will end with Continue/Break)
  | For [Stmt] [Stmt]
  -- dummy statement because I am a dummy
  | ForStepLabel
  | Continue
  | Break
  | Block [Stmt]

type Op = AST.Op

instance Show Function where
  show :: Function -> String
  show fun = "fun " ++ name fun ++ ":" ++ show (code fun)

instance Show Stmt where
  show :: Stmt -> String
  show (Decl typ nam) = "decl " ++ show typ ++ " " ++ nam ++ "\n"
  show (Discard nam) = "discard " ++ " " ++ nam ++ "\n"
  show (Asgn nam e) = "asgn " ++ " " ++ nam ++ " = " ++ show e ++ "\n"
  show (Ret lit) = "ret " ++ " " ++ show lit
  show (If cond ifB elseB) = "if (" ++ show cond ++ ") {\n" ++ show ifB ++ "} else {\n" ++ show elseB ++ "}\n"
  show (For init body) = "for, init =\n" ++ show init ++ " body =\n" ++ show body ++ "\n"
  show ForStepLabel = "forStepLabel\n"
  show Continue = "continue\n"
  show Break = "break\n"
  show (Block ss) = "{\n" ++ show ss ++ "}\n"
