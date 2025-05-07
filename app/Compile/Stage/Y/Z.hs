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
function' fun = Function (Z.name fun) (stmts (execState (forM_ (Z.code fun) stmt') initialState))
    where initialState = CodeGenState 0 []

stmt' :: Z.Stmt -> CodeGen ()
stmt' (Z.Decl name) = modify $ \s -> s { stmts = stmts s ++ [ Decl name ] }
stmt' (Z.Init name e) = do
    e' <- expr' e
    modify $ \s -> s { stmts = stmts s ++ [ Decl name, Asgn name e' ] }
    return ()
stmt' (Z.Asgn name op e) = case op of
    Nothing -> do
        e' <- expr' e
        modify $ \s -> s { stmts = stmts s ++ [ Asgn name e' ]}
        return ()
    Just opYes -> do
        e' <- expr' (Z.BinExpr opYes (Z.Ident name) e)
        modify $ \s -> s { stmts = stmts s ++ [ Asgn name e' ]}
        return ()
stmt' (Z.Ret e) = do
    e' <- makePlain' e
    modify $ \s -> s { stmts = stmts s ++ [ Ret e' ]}
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
    curr <- get
    let fresh = "temp_" ++ show (nextTemp curr)
    modify $ \s -> s { nextTemp = nextTemp curr + 1 }
    e' <- expr' (Z.UnExpr op e)
    modify $ \s -> s { stmts = stmts s ++ [ Decl fresh, Asgn fresh e' ] }
    return $ Ident fresh
makePlain' (Z.BinExpr op e1 e2) = do
    curr <- get
    let fresh = "temp_" ++ show (nextTemp curr)
    let fresh1 = "temp_" ++ show (nextTemp curr + 1)
    let fresh2 = "temp_" ++ show (nextTemp curr + 2)
    modify $ \s -> s { nextTemp = nextTemp curr + 3 }
    e1' <- makePlain' e1
    e2' <- makePlain' e2
    modify $ \s -> s { stmts = stmts s ++ [ Decl fresh1, Asgn fresh1 (Plain e1') ] }
    modify $ \s -> s { stmts = stmts s ++ [ Decl fresh2, Asgn fresh2 (Plain e2') ] }
    modify $ \s -> s { stmts = stmts s ++ [ Decl fresh, Asgn fresh (BinExpr op (Ident fresh1) (Ident fresh2)) ] }
    return $ Ident fresh
