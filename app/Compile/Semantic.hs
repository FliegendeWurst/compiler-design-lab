module Compile.Semantic
  ( semanticAnalysis
  ) where

import           Compile.AST (AST(..), Expr(..), Stmt(..), Simp(..), Ctrl(..), posPretty, HexOrDecInteger (..), intValue, ExprType (..))
import           Error (L1ExceptT, semanticFail)

import           Control.Monad (unless, when)
import           Control.Monad.State
import qualified Data.Map as Map
import Util (unwrap)
import Prelude hiding (init)
import Data.Maybe (catMaybes)

data VariableStatus
  = Declared ExprType
  | Initialized ExprType
  deriving (Show, Eq)

data Namespace = Ns {
  scope :: Map.Map String VariableStatus,
  parent :: Maybe Namespace,
  kind :: ScopeType
}

data ScopeType
  = Plain
  | Loop
  deriving (Eq)

type L1Semantic = StateT Namespace L1ExceptT

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> L1Semantic a
semanticFail' = lift . semanticFail

semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
  ns <- varStatusAnalysis ast
  evalStateT (checkReturns ast) ns

-- right now an AST is just a list of statements
varStatusAnalysis :: AST -> L1ExceptT Namespace
varStatusAnalysis (Function stmts _) = do
  execStateT (mapM_ checkStmt stmts) $ Ns Map.empty Nothing Plain

isInLoop :: Namespace -> Bool
isInLoop ns =
  thisScope || parentScope
  where
    thisScope = kind ns == Loop
    parentScope = maybe False isInLoop (parent ns)

isDeclared :: Namespace -> String -> Bool
isDeclared ns name =
  thisScope || parentScope
  where
    thisScope = Map.member name (scope ns)
    parentScope = case parent ns of
      Just parentNs -> isDeclared parentNs name
      Nothing -> False

isInitialized :: Namespace -> String -> Maybe ExprType
isInitialized ns name =
  maybe Nothing (head $ catMaybes [thisScope, parentScope])
  where
    thisScope = case Map.lookup name (scope ns) of
      Just (Initialized typ) -> Just typ
      _ -> Nothing
    parentScope = case parent ns of
      Just parentNs -> isInitialized parentNs name
      Nothing -> Nothing

makeDeclared :: String -> L1Semantic ()
makeDeclared name = do
  modify $ \s -> s { scope = Map.insert name Declared (scope s) }

makeInitialized :: String -> L1Semantic ()
makeInitialized name = do
  modify $ \s -> s { scope = Map.insert name Initialized (scope s) }

pushScope :: L1Semantic ()
pushScope = do
  modify $ \s -> s {
    scope = Map.empty,
    parent = Just s
  }

popScope :: L1Semantic ()
popScope = do
  modify $ \s -> s {
    scope = scope $ unwrap $ parent s,
    parent = parent $ unwrap $ parent s
  }

checkStmts :: [Stmt] -> L1Semantic ()
checkStmts [] = pure ()
checkStmts [x] = checkStmt x
checkStmts (x:y) = do
  checkStmt x
  checkStmts y

-- So far this checks:
-- + we cannot declare a variable again that has already been declared or initialized
-- + we cannot initialize a variable again that has already been declared or initialized
-- + a variable needs to be declared or initialized before we can assign to it
-- + we can only return valid expressions
checkStmt :: Stmt -> L1Semantic ()
checkStmt (Simple (Decl typ name pos)) = do
  ns <- get
  when (isDeclared ns name)
    $ semanticFail'
    $ "Variable " ++ name ++ " redeclared at: " ++ posPretty pos
  makeDeclared name
checkStmt (Simple (Init typ name e pos)) = do
  ns <- get
  when (isDeclared ns name)
    $ semanticFail'
    $ "Variable " ++ name ++ " redeclared (initialized) at: " ++ posPretty pos
  checkExpr e
  makeInitialized name
checkStmt (Simple (Asgn name Nothing e pos)) = do
  ns <- get
  unless (isDeclared ns name)
    $ semanticFail'
    $ "Trying to assign to undeclared variable "
        ++ name
        ++ " at: "
        ++ posPretty pos
  checkExpr e
  makeInitialized name
checkStmt (Simple (Asgn name (Just _) e pos)) = do
  ns <- get
  unless (isInitialized ns name)
    $ semanticFail'
    $ "Trying to op-assign to uninitialized variable "
        ++ name
        ++ " at: "
        ++ posPretty pos
  checkExpr e
  makeInitialized name
checkStmt (Control (If cond ifB elseB)) = do
  checkExpr cond
  pushScope
  checkStmt ifB
  popScope
  case elseB of
    Just elseStmt -> do
      pushScope
      checkStmt elseStmt
      popScope
    Nothing -> pure ()
checkStmt (Control (While cond body)) = do
  checkExpr cond
  pushScope
  checkStmt body
  popScope
checkStmt (Control (For init cond step body)) = do
  pushScope
  case init of
    Just x -> do
      checkStmt $ Simple x
    Nothing -> pure ()
  checkExpr cond
  case step of
    Just x -> do
      checkStmt $ Simple x
    Nothing -> pure ()
  checkStmt body
  popScope
checkStmt (Control Continue) = do
  ns <- get
  unless (isInLoop ns)
    $ semanticFail' "continue not in loop"
checkStmt (Control Break) = do
  ns <- get
  unless (isInLoop ns)
    $ semanticFail' "break not in loop"
checkStmt (Control (Ret e _)) = checkExpr e
checkStmt (Block body) = do
  pushScope
  checkStmts body
  popScope

--checkExprOpt :: Maybe Expr -> L1Semantic ()
--checkExprOpt (Just x) = checkExpr x
--checkExprOpt Nothing = pure ()

checkExpr :: Expr -> ExprType -> L1Semantic ()
checkExpr (IntExpr n pos) IntT = do
  when (invalidIntegerLiteral n)
    $ semanticFail'
    $ "Integer literal " ++ show (intValue n) ++ " out of bounds at: " ++ posPretty pos
checkExpr (IntExpr n pos) BoolT = do
  semanticFail'
    $ "Integer literal used as boolean at: " ++ posPretty pos
checkExpr (BoolLit x) BoolT = do
  return ()
checkExpr (BoolLit x) IntT = do
  semanticFail' "Boolean literal used as integer"
checkExpr (Ident name pos) t = do
  ns <- get
  unless (isInitialized ns name)
    $ semanticFail'
    $ "Variable "
            ++ name
            ++ " used without initialization at: "
            ++ posPretty pos
checkExpr (UnExpr _ e) t = checkExpr e t
checkExpr (BinExpr _ lhs rhs) t = (checkExpr lhs t) >> (checkExpr rhs t) -- FIXME

invalidIntegerLiteral :: HexOrDecInteger -> Bool
invalidIntegerLiteral (Dec x) = x < 0 || x >2^(31 :: Integer)
invalidIntegerLiteral (Hex x) = x < 0 || x > 0xffffffff

checkReturns :: AST -> L1Semantic ()
checkReturns (Function stmts _) = do
  let returns = any isReturn stmts
  unless returns $ semanticFail' "Program does not return"
  where
    isReturn (Ret _ _) = True
    isReturn _ = False
