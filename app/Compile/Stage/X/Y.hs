module Compile.Stage.X.Y
    ( fromYToX
    ) where
import Control.Monad.State
import Control.Monad (forM_)
import Compile.IR.Y (Y, Expr (..), LitOrIdent (..))
import Compile.IR.X (X,Function, Stmt (..))
import qualified Compile.IR.Y as Y
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Compile.IR.X as X
import Prelude hiding (init)

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { lastOccurence :: Map String Integer
  , stmts :: [Stmt]
  }

fromYToX :: Y -> X
fromYToX = map function'

function' :: Y.Function -> Function
function' fun = X.Function (Y.name fun) (reverse (stmts (execState (doFunction' fun) initialState)))
    where initialState = CodeGenState Map.empty []

doFunction' :: Y.Function -> CodeGen ()
doFunction' fun = do
    forM_ (zip (Y.code fun) [ 1 .. ]) stmt'
    lasts <- gets lastOccurence
    let indexed = Map.fromListWith (++) $ map flipTuple $ Map.assocs lasts
    forM_ (zip (Y.code fun) [ 1 .. ]) (doStmt' indexed)

flipTuple :: (a, b) -> (b, [a])
flipTuple (a, b) = (b, [a])

stmt' :: (Y.Stmt, Integer) -> CodeGen ()
stmt' (Y.Decl _ name, idx) = do
    registerLast name idx
stmt' (Y.Asgn name e, idx) = do
    registerLast name idx
    registerExpr e idx
stmt' (Y.Ret e, idx) = do
    registerExpr (Plain e) idx
stmt' (Y.If cond ifB elseB, idx) = do
  registerExpr (Plain cond) idx
  return $ error "If to X"
stmt' (Y.For init body cond, idx) = do
  registerExpr cond idx -- FIXME after continue / end of loop body
stmt' (Y.ForStepLabel, _) = do
  -- FIXME see above
  return ()
stmt' (Y.Continue, idx) = pure () -- ?
stmt' (Y.Break, idx) = pure () -- ?
stmt' (Y.Block ss, idx) = do
  -- FIXME
  return $ error "block to X"

doStmt' :: Map Integer [String] -> (Y.Stmt, Integer) -> CodeGen ()
doStmt' indexed (stmt, idx) = do
    modify $ \s -> s { stmts = stmtIdentity stmt : stmts s }
    let toDrop = Map.lookup idx indexed
    case toDrop of
        Just x -> forM_ x addDropStmt
        Nothing -> return ()

addDropStmt :: String -> CodeGen ()
addDropStmt x =
    modify $ \s -> s { stmts = Discard x : stmts s }

stmtIdentity :: Y.Stmt -> Stmt
stmtIdentity (Y.Decl t x) = Decl t x
stmtIdentity (Y.Asgn name e) = Asgn name e
stmtIdentity (Y.Ret e) = Ret e
stmtIdentity (Y.If cond ifB elseB) = If cond (stmtIdentity ifB) (stmtIdentity elseB)
stmtIdentity (Y.For init body _) = For (map stmtIdentity init) $ map stmtIdentity body
stmtIdentity Y.ForStepLabel = ForStepLabel
stmtIdentity Y.Continue = Continue
stmtIdentity Y.Break = Break
stmtIdentity (Y.Block ss) = Block $ map stmtIdentity ss

registerLast :: String -> Integer -> CodeGen ()
registerLast name idx = do
    modify $ \s -> s { lastOccurence = Map.insert name idx (lastOccurence s) }

registerExpr :: Expr -> Integer -> CodeGen ()
registerExpr e idx = do
    case e of
        Plain (Lit _) -> return ()
        Plain (LitB _) -> return ()
        Plain (Ident x) -> registerLast x idx
        UnExpr _ (Lit _) -> return ()
        UnExpr _ (LitB _) -> return ()
        UnExpr _ (Ident x) -> registerLast x idx
        BinExpr _ e1 e2 -> do
            case e1 of
                Ident x -> registerLast x idx
                _ -> return ()
            case e2 of
                Ident x -> registerLast x idx
                _ -> return ()
