module Compile.Stage.Z.AST
    ( fromASTToZ
    ) where
import Compile.AST (AST, intValue)
import Compile.IR.Z (Z, Function (Function), Stmt (..), Expr (..), BinOp (..), UnOp (..))
import qualified Compile.AST as AST

fromASTToZ :: AST -> Z
fromASTToZ (AST.Function stmts _) = [function' "main" stmts]

function' :: String -> [ AST.Stmt ] -> Function
function' name stmts = Function name (map stmt' stmts)

stmt' :: AST.Stmt -> Stmt
stmt' (AST.Decl name _) = Decl name
stmt' (AST.Init name e _) = Init name (expr' e)
stmt' (AST.Asgn name op e _) = Asgn name (binOp' op) (expr' e)
stmt' (AST.Ret e _) = Ret (expr' e)

expr' :: AST.Expr -> Expr
expr' (AST.IntExpr garbage _) = Lit $ fromInteger $ intValue garbage
expr' (AST.Ident name _) = Ident name
expr' (AST.UnExpr op e) = UnExpr (unOp' op) (expr' e)
expr' (AST.BinExpr op e1 e2) = BinExpr (unwrap $ binOp' $ Just op) (expr' e1) (expr' e2)

binOp' :: AST.AsgnOp -> Maybe BinOp
binOp' (Just AST.Add) = Just Add
binOp' (Just AST.Sub) = Just Sub
binOp' (Just AST.Mul) = Just Mul
binOp' (Just AST.Div) = Just Div
binOp' (Just AST.Mod) = Just Mod
binOp' (Just AST.BitwiseAnd) = Just BitwiseAnd
binOp' (Just AST.BitwiseXor) = Just BitwiseXor
binOp' (Just AST.BitwiseOr) = Just BitwiseOr
binOp' (Just AST.LogicalAnd) = Just LogicalAnd
binOp' (Just AST.LogicalOr) = Just LogicalOr
binOp' (Just AST.LeftShift) = Just LeftShift
binOp' (Just AST.RightShift) = Just RightShift
binOp' (Just AST.IntLt) = Just IntLt
binOp' (Just AST.IntLe) = Just IntLe
binOp' (Just AST.IntGt) = Just IntGt
binOp' (Just AST.IntGe) = Just IntGe
binOp' (Just AST.Equals) = Just Equals
binOp' (Just AST.EqualsNot) = Just EqualsNot
binOp' (Just AST.Ternary1) = Just Ternary1
binOp' (Just AST.Ternary2) = Just Ternary2
binOp' Nothing = Nothing
binOp' x = error $ "illegal binary operator " ++ show x

unOp' :: AST.Op -> UnOp
unOp' AST.Neg = Neg
unOp' AST.LogicalNot = LogicalNot
unOp' AST.BitwiseNot = BitwiseNot
unOp' x = error $ "illegal unary operator " ++ show x

