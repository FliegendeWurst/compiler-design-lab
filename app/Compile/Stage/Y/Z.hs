module Compile.Stage.Y.Z
    ( fromZToY
    ) where
import Compile.IR.Y (Y, Function (Function), Stmt (..), Expr (..), LitOrIdent (..))
import Compile.IR.Z (Z, typeExpr)
import qualified Compile.IR.Z as Z
import Control.Monad.State
import Control.Monad (forM_)
import Compile.AST (ExprType(..))
import Data.Maybe (fromMaybe)
import Prelude hiding (init)
import Data.Map (Map)
import qualified Data.Map as Map

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { nextTemp :: Integer
  , stmts :: [Stmt]
  , parentScopes :: [[Stmt]]
  , typs :: Map String ExprType
  , parentTyps :: [Map String ExprType]
  }

push :: Stmt -> CodeGen ()
push stmt = do
  case stmt of
    Decl t name -> do
      modify $ \s -> s { typs = Map.insert name t (typs s) }
    _ -> pure ()
  modify $ \s -> s { stmts = stmt : stmts s }

pushScope :: CodeGen ()
pushScope = do
  modify $ \s -> s {
    stmts = [],
    parentScopes = stmts s : parentScopes s,
    typs = Map.empty,
    parentTyps = typs s : parentTyps s
  }

popScope :: CodeGen [Stmt]
popScope = do
  ss <- gets stmts
  modify $ \s -> s {
    stmts = head (parentScopes s),
    parentScopes = tail (parentScopes s),
    typs = head (parentTyps s),
    parentTyps = tail (parentTyps s)
  }
  return ss

fromZToY :: Z -> Y
fromZToY = map function'

function' :: Z.Function -> Function
function' fun = Function (Z.name fun) (stmts (execState (doFunction' fun) initialState))
  where initialState = CodeGenState 0 [] [] Map.empty []

doFunction' :: Z.Function -> CodeGen ()
doFunction' fun = do
    forM_ (Z.code fun) stmt'
    modify $ \s -> s { stmts = reverse (stmts s) }

stmt' :: Z.Stmt -> CodeGen ()
stmt' (Z.Simple (Z.Decl t name)) = do
  push $ Decl t name
  return ()
stmt' (Z.Simple (Z.Init t name e)) = do
    e' <- expr' e
    push $ Decl t name
    push $ Asgn name e'
    return ()
stmt' (Z.Simple (Z.Asgn name op e)) = case op of
    Nothing -> do
        e' <- expr' e
        modify $ \s -> s { stmts = Asgn name e' : stmts s }
        return ()
    Just opYes -> do
        e' <- expr' (Z.BinExpr opYes (Z.Ident name) e)
        modify $ \s -> s { stmts = Asgn name e' : stmts s }
        return ()
stmt' (Z.If cond ifB elseB) = do
  e' <- expr' cond
  fresh <- freshTemp
  push $ Decl BoolT fresh
  push $ Asgn fresh e'

  pushScope
  stmt' ifB
  ifB' <- popScope
  pushScope
  stmt' elseB
  elseB' <- popScope

  push $ If (Ident fresh) (Block (reverse ifB')) (Block (reverse elseB'))
  return ()
stmt' (Z.For init cond step body) = do
  condV <- freshTemp

  pushScope
  stmt' $ maybe (Z.Block []) Z.Simple init
  e' <- expr' cond
  push $ Asgn condV e'
  push $ If (Ident condV) Continue Break
  init' <- popScope

  pushScope
  stmt' body
  stmt' $ maybe (Z.Block []) Z.Simple step
  e2' <- expr' cond
  push $ Asgn condV e2'
  push $ If (Ident condV) Continue Break
  body' <- popScope

  push $ For init' body'
stmt' Z.Continue = push Continue
stmt' Z.Break = push Break
stmt' (Z.Block stmtsB) = do
  mapM_ stmt' stmtsB
stmt' (Z.Ret e) = do
    e' <- makePlain' e
    modify $ \s -> s { stmts = Ret e' : stmts s }
    return ()

expr' :: Z.Expr -> CodeGen Expr
expr' (Z.Lit value) = return $ Plain $ Lit value
expr' (Z.LitB value) = return $ Plain $ LitB value
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
makePlain' (Z.LitB value) = return $ LitB value
makePlain' (Z.Ident name) = return $ Ident name
makePlain' (Z.UnExpr op e) = do
    fresh <- freshTemp
    e' <- expr' (Z.UnExpr op e)
    ctx <- gets typs
    let t' = typeExpr ctx (Z.UnExpr op e)
    modify $ \s -> s { stmts = Asgn fresh e' : Decl t' fresh : stmts s }
    return $ Ident fresh
makePlain' (Z.BinExpr op e1 e2) = do
    fresh <- freshTemp
    fresh1 <- freshTemp
    fresh2 <- freshTemp
    e1' <- makePlain' e1
    e2' <- makePlain' e2
    ctx <- gets typs
    let t1' = typeExpr ctx e1
    let t2' = typeExpr ctx e2
    let t' = typeExpr ctx (Z.BinExpr op e1 e2)
    modify $ \s -> s { stmts = Asgn fresh1 (Plain e1') : Decl t1' fresh1 : stmts s }
    modify $ \s -> s { stmts = Asgn fresh2 (Plain e2') : Decl t2' fresh2 : stmts s }
    modify $ \s -> s { stmts = Asgn fresh (BinExpr op (Ident fresh1) (Ident fresh2)) : Decl t' fresh : stmts s }
    return $ Ident fresh

freshTemp :: CodeGen String
freshTemp = do
    curr <- gets nextTemp
    modify $ \s -> s { nextTemp = curr + 1 }
    return $ freshPrefix ++ show curr

freshPrefix :: String
freshPrefix = "."
