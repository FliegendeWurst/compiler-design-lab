module Compile.Stage.Z.AST
    ( fromASTToZ
    ) where
import Compile.AST (AST, intValue)
import Compile.IR.Z (Z, Function (Function), Stmt (..), Expr (..))
import qualified Compile.AST as AST

fromASTToZ :: AST -> Z
fromASTToZ (AST.Block stmts _) = [function' "main" stmts]

function' :: String -> [ AST.Stmt ] -> Function
function' name stmts = Function name (map stmt' stmts)

stmt' :: AST.Stmt -> Stmt
stmt' (AST.Decl name _) = Decl name
stmt' (AST.Init name e _) = Init name (expr' e)
stmt' (AST.Asgn name op e _) = Asgn name op (expr' e)
stmt' (AST.Ret e _) = Ret (expr' e)

expr' :: AST.Expr -> Expr
expr' (AST.IntExpr garbage _) = Lit (intValue garbage)
expr' (AST.Ident name _) = Ident name
expr' (AST.UnExpr op e) = UnExpr op (expr' e)
expr' (AST.BinExpr op e1 e2) = BinExpr op (expr' e1) (expr' e2)