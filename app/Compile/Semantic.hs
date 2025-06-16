module Compile.Semantic
  ( semanticAnalysis
  ) where

import           Compile.AST (AST(..), Expr(..), Stmt(..), Simp(..), Ctrl(..), posPretty, HexOrDecInteger (..), intValue, ExprType (..), opIsIntIntToInt, opIsIntIntToBool, opIsBoolBoolToBool, typeExpr)
import           Error (L1ExceptT, semanticFail)

import           Control.Monad (unless, when, forM_)
import           Control.Monad.State
import qualified Data.Map as Map
import Util (expect, headM)
import Prelude hiding (init)
import Data.Maybe (catMaybes, isNothing, isJust)
import Data.Map (Map)

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

isDeclared :: Namespace -> String -> Maybe ExprType
isDeclared ns name =
  headM $ catMaybes [thisScope, parentScope]
  where
    thisScope = case Map.lookup name (scope ns) of
      Just (Initialized typ) -> Just typ
      Just (Declared typ) -> Just typ
      _ -> Nothing
    parentScope = case parent ns of
      Just parentNs -> isDeclared parentNs name
      Nothing -> Nothing

isInitialized :: Namespace -> String -> Maybe ExprType
isInitialized ns name =
  headM $ catMaybes [thisScope, parentScope]
  where
    thisScope = case Map.lookup name (scope ns) of
      Just (Initialized typ) -> Just typ
      _ -> Nothing
    parentScope = case parent ns of
      Just parentNs -> isInitialized parentNs name
      Nothing -> Nothing

getTypeContext :: Namespace -> Map String ExprType
getTypeContext ns =
  Map.union thisT parentT
  where
    thisT = Map.map (\x -> case x of
      Initialized typ -> typ
      Declared typ -> typ) (scope ns)
    parentT = maybe Map.empty getTypeContext (parent ns)

makeDeclared :: String -> ExprType-> L1Semantic ()
makeDeclared name typ = do
  modify $ \s -> s { scope = Map.insert name (Declared typ) (scope s) }

makeInitialized :: String -> ExprType -> L1Semantic ()
makeInitialized name typ = do
  modify $ \s -> s { scope = Map.insert name (Initialized typ) (scope s) }

pushScope :: L1Semantic ()
pushScope = do
  modify $ \s -> s {
    scope = Map.empty,
    parent = Just s
  }

popScope :: L1Semantic ()
popScope = do
  modify $ \s -> s {
    scope = scope $ expect "failed to pop semantic scope" $ parent s,
    parent = parent $ expect "failed to pop semantic scope" $ parent s
  }

checkStmts :: [Stmt] -> L1Semantic ()
checkStmts [] = pure ()
checkStmts [x] = checkStmt x
checkStmts (x:y) = do
  checkStmt x
  checkStmts y

findDeclared :: Namespace -> Map String ExprType
findDeclared ns =
  Map.union thisT parentT
  where
    thisT = findDeclared' (scope ns)
    parentT = maybe Map.empty findDeclared (parent ns)

findDeclared' :: Map.Map String VariableStatus -> Map String ExprType
findDeclared' = Map.foldrWithKey (\ nam status xs -> case status of
    Declared typ -> Map.insert nam typ xs
    _ -> xs
    ) Map.empty

findInitialized :: Map.Map String VariableStatus -> Map String ExprType
findInitialized = Map.foldrWithKey (\ nam status xs -> case status of
    Initialized typ -> Map.insert nam typ xs
    _ -> xs
    ) Map.empty

-- So far this checks:
-- + we cannot declare a variable again that has already been declared or initialized
-- + we cannot initialize a variable again that has already been declared or initialized
-- + a variable needs to be declared or initialized before we can assign to it
-- + we can only return valid expressions
-- (outdated)
checkStmt :: Stmt -> L1Semantic ()
checkStmt (Simple (Decl typ name pos)) = do
  ns <- get
  when (isJust (isDeclared ns name))
    $ semanticFail'
    $ "Variable " ++ name ++ " redeclared at: " ++ posPretty pos
  makeDeclared name typ
checkStmt (Simple (Init typ name e pos)) = do
  ns <- get
  when (isJust (isDeclared ns name))
    $ semanticFail'
    $ "Variable " ++ name ++ " redeclared (initialized) at: " ++ posPretty pos
  checkExpr e typ
  makeInitialized name typ
checkStmt (Simple (Asgn name Nothing e pos)) = do
  ns <- get
  let typ = isDeclared ns name
  when (isNothing typ)
    $ semanticFail'
    $ "Trying to assign to undeclared variable "
        ++ name
        ++ " at: "
        ++ posPretty pos
  checkExpr e $ expect "impossible" typ
  makeInitialized name $ expect "impossible" typ
checkStmt (Simple (Asgn name (Just _) e pos)) = do
  ns <- get
  let typ = isInitialized ns name
  when (isNothing typ)
    $ semanticFail'
    $ "Trying to op-assign to uninitialized variable "
        ++ name
        ++ " at: "
        ++ posPretty pos
  checkExpr e $ expect "impossible" typ
  makeInitialized name $ expect "impossible" typ
checkStmt (Control (If cond ifB elseB)) = do
  checkExpr cond BoolT
  ns <- get
  pushScope
  checkStmt ifB
  def1 <- gets scope
  popScope
  pushScope
  checkStmt elseB
  def2 <- gets scope
  popScope
  -- Important: merge commonly defined variables, and define them in parent scope!
  let iP = findDeclared ns
  let i1 = findInitialized def1
  let i2 = findInitialized def2
  let ic = Map.intersection i1 i2
  let icc = Map.intersection ic iP
  forM_ (Map.assocs icc) (\e -> do
    uncurry makeInitialized e
    )
checkStmt (Control (While cond body)) = do
  checkExpr cond BoolT
  pushScope
  checkStmt body
  popScope
checkStmt (Control (For init cond step body)) = do
  case step of
    (Just (Decl {})) -> do
      semanticFail' "step statement may not be declaration"
    (Just (Init {})) -> do
      semanticFail' "step statement may not be initialization" -- FIXME confirm
    _ -> pure ()
  pushScope
  case init of
    Just x -> do
      checkStmt $ Simple x
    Nothing -> pure ()
  checkExpr cond BoolT
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
checkStmt (Control (Ret e _)) = checkExpr e IntT
checkStmt (Block body) = do
  ns <- get
  pushScope
  checkStmts body
  def <- gets scope
  popScope
  let iP = findDeclared ns
  let i1 = findInitialized def
  let icc = Map.intersection i1 iP
  forM_ (Map.assocs icc) (\e -> do
    uncurry makeInitialized e
    )

--checkExprOpt :: Maybe Expr -> L1Semantic ()
--checkExprOpt (Just x) = checkExpr x
--checkExprOpt Nothing = pure ()

checkExpr :: Expr -> ExprType -> L1Semantic ()
checkExpr (IntExpr n pos) IntT = do
  when (invalidIntegerLiteral n)
    $ semanticFail'
    $ "Integer literal " ++ show (intValue n) ++ " out of bounds at: " ++ posPretty pos
checkExpr (IntExpr _ pos) BoolT = do
  semanticFail'
    $ "Integer literal used as boolean at: " ++ posPretty pos
checkExpr (BoolLit _) BoolT = do
  return ()
checkExpr (BoolLit _) IntT = do
  semanticFail' "Boolean literal used as integer"
checkExpr (Ident name pos) t = do
  ns <- get
  let typ = isInitialized ns name
  when (isNothing typ)
    $ semanticFail'
    $ "Variable "
            ++ name
            ++ " used without initialization at: "
            ++ posPretty pos
  when (typ /= Just t)
    $ semanticFail'
    $ "Variable "
            ++ name
            ++ " used for wrong type at: "
            ++ posPretty pos
checkExpr (UnExpr _ e) t = checkExpr e t
-- Cases:
-- int · int → int
-- int · int → bool
-- bool · bool → bool
-- AND THE STUPID TERNARY!!!
checkExpr (BinExpr op lhs rhs) t = do
  ns <- get
  let ts = getTypeContext ns
  let intIntToInt = opIsIntIntToInt op
  case t of
    IntT -> do
      unless intIntToInt $ semanticFail' "expected integer operator"
      checkExpr lhs IntT
      checkExpr rhs IntT
    BoolT -> do
      do
        when (typeExpr ts lhs == IntT) $ do
          checkExpr lhs IntT
          checkExpr rhs IntT
      do
        when (typeExpr ts lhs == BoolT) $ do
          checkExpr lhs BoolT
          checkExpr rhs BoolT
  -- FIXME: ternary

invalidIntegerLiteral :: HexOrDecInteger -> Bool
invalidIntegerLiteral (Dec x) = x < 0 || x >2^(31 :: Integer)
invalidIntegerLiteral (Hex x) = x < 0 || x > 0xffffffff

checkReturns :: AST -> L1Semantic ()
checkReturns (Function stmts _) = do
  let returns = any isReturn stmts
  unless returns $ semanticFail' "Program does not return"

isReturn :: Stmt -> Bool
isReturn (Control (Ret _ _)) = True
isReturn (Control (If _ ifB elseB)) = isReturn ifB && isReturn elseB
isReturn (Block ss) = any isReturn ss
isReturn _ = False
