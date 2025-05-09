module Compile.Stage.Y.Z
    ( fromZToY
    ) where
import Compile.IR.Y (Y, Function (Function), Stmt (..), Expr (..), LitOrIdent (..))
import Compile.IR.Z (Z)
import qualified Compile.IR.Z as Z
import Control.Monad.State
import Control.Monad (forM_)

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { nextTemp :: Integer
  , stmts :: [Stmt]
  }

fromZToY :: Z -> Y
fromZToY = map function'

function' :: Z.Function -> Function
function' fun = Function (Z.name fun) (stmts (execState (doFunction' fun) initialState))
    where initialState = CodeGenState 0 []

doFunction' :: Z.Function -> CodeGen ()
doFunction' fun = do
    forM_ (Z.code fun) stmt'
    modify $ \s -> s { stmts = reverse (stmts s) }

stmt' :: Z.Stmt -> CodeGen ()
stmt' (Z.Decl name) = modify $ \s -> s { stmts = Decl name : stmts s }
stmt' (Z.Init name e) = do
    e' <- expr' e
    modify $ \s -> s { stmts = Asgn name e' : Decl name : stmts s }
    return ()
stmt' (Z.Asgn name op e) = case op of
    Nothing -> do
        e' <- expr' e
        modify $ \s -> s { stmts = Asgn name e' : stmts s }
        return ()
    Just opYes -> do
        e' <- expr' (Z.BinExpr opYes (Z.Ident name) e)
        modify $ \s -> s { stmts = Asgn name e' : stmts s }
        return ()
stmt' (Z.Ret e) = do
    e' <- makePlain' e
    modify $ \s -> s { stmts = Ret e' : stmts s }
    return ()

expr' :: Z.Expr -> CodeGen Expr
expr' (Z.Lit value) = return $ Plain $ Lit value
expr' (Z.Ident name) = return $ Plain $ Ident name
expr' (Z.UnExpr op e) = do
    e' <- makePlain' e
    return $ UnExpr op e'
expr' (Z.BinExpr op e1 e2) = do
    e1' <- makePlain' e1
    e2' <- makePlain' e2
    return $ BinExpr op e1' e2'

makePlain' :: Z.Expr -> CodeGen LitOrIdent
makePlain' (Z.Lit value) = return $ Lit value
makePlain' (Z.Ident name) = return $ Ident name
makePlain' (Z.UnExpr op e) = do
    fresh <- freshTemp
    e' <- expr' (Z.UnExpr op e)
    modify $ \s -> s { stmts = Asgn fresh e' : Decl fresh : stmts s }
    return $ Ident fresh
makePlain' (Z.BinExpr op e1 e2) = do
    fresh <- freshTemp
    fresh1 <- freshTemp
    fresh2 <- freshTemp
    e1' <- makePlain' e1
    e2' <- makePlain' e2
    modify $ \s -> s { stmts = Asgn fresh1 (Plain e1') : Decl fresh1 : stmts s }
    modify $ \s -> s { stmts = Asgn fresh2 (Plain e2') : Decl fresh2 : stmts s }
    modify $ \s -> s { stmts = Asgn fresh (BinExpr op (Ident fresh1) (Ident fresh2)) : Decl fresh : stmts s }
    return $ Ident fresh

freshTemp :: CodeGen String
freshTemp = do
    curr <- gets nextTemp
    modify $ \s -> s { nextTemp = curr + 1 }
    return $ freshPrefix ++ show curr

freshPrefix :: String
freshPrefix = "."
