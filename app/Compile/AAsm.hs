{-# LANGUAGE QuasiQuotes #-}

module Compile.AAsm
  ( codeGen
  ) where

import           Compile.AST (AST(..), Expr(..), Stmt(..), showAsgnOp, Op (..))

import           Control.Monad.State
import qualified Data.Map as Map

import TH (litFile)
import Error (parserFail)

type Register = Integer

type VarName = String
type TempVarIndex = Integer

type AAsmAlloc = Map.Map VarName Register

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: AAsmAlloc
  , nextReg :: Register
  , nextTempVar :: Integer
  , code :: [String]
  }

assemblyTemplate :: String
assemblyTemplate = [litFile|app/Compile/assembly_template.s|]

simplifyStmts :: [Stmt] -> CodeGen [Stmt]
simplifyStmts [ ] = return [ ]
simplifyStmts ( x : xs ) = do
  newHead <- case x of
    (Decl _ _) -> return [ x ]
    (Init _ (Ident _ _) _) -> return [ x ]
    (Init _ (IntExpr _ _) _) -> return [ x ]
    _ -> return [ x ]

  simplifyWhatExtra <- if length newHead == 1 then return [ ] else return newHead
  simplifyDone <- if length newHead == 1 then return newHead else return []
  rest <- simplifyStmts $ simplifyWhatExtra ++ xs
  return $ simplifyDone ++ rest


codeGen :: AST -> [String]
codeGen (Block stmts _) = lines assemblyTemplate ++ code (execState (genBlock stmts) initialState)
  where
    initialState = CodeGenState Map.empty 0 0 []

--codeGen :: AST -> [String]
--codeGen ast = code (execState (codeGen2 ast) initialState)
--  where initialState = CodeGenState Map.empty 0 0 []
--
--codeGen2 :: AST -> CodeGen [String]
--codeGen2 (Block stmts _) = do
--  -- stmtsSimple <- simplifyStmts stmts
--  genBlock  stmts --stmtsSimple
--  curr <- get
--  return $ lines assemblyTemplate ++ code curr

regName :: Register -> String
regName 0 = "eax"
regName 1 = "ebx"
regName 2 = "ecx"
regName 3 = "edx"
regName 4 = "esi"
regName 5 = "edi"
regName 6 = "ebp"
regName 7 = "esp"
regName 8 = "r8d"
regName 9 = "r9d"
regName 10 = "r10d"
regName 11 = "r11d"
regName 12 = "r12d"
regName 13 = "r13d"
regName 14 = "r14d"
regName 15 = "r15d"
-- TODO: use xmm registers :)
regName _ = error "out of registers :("

freshReg :: CodeGen Register
freshReg = do
  curr <- get
  let r = nextReg curr
  put curr {nextReg = r + 1}
  return r

freshTempVar :: CodeGen VarName
freshTempVar = do
  curr <- get
  let r = nextTempVar curr
  put curr {nextTempVar = r + 1}
  return ("tempCompilerInternal" ++ show r)


assignVar :: VarName -> Register -> CodeGen ()
assignVar name r = do
  modify $ \s -> s {regMap = Map.insert name r (regMap s)}

lookupVar :: VarName -> CodeGen Register
lookupVar name = do
  m <- gets regMap
  case Map.lookup name m of
    Just r -> return r
    Nothing -> error "unreachable, fix your semantic analysis I guess"

emit :: String -> CodeGen ()
emit instr = modify $ \s -> s {code = code s ++ [instr]}

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> CodeGen ()
genStmt (Decl name _) = do
  r <- freshReg
  assignVar name r
genStmt (Init name e _) = do
  r <- genExpr e
  assignVar name r
genStmt (Asgn name op e _) = do
  rhs <- genExpr e
  lhs <- lookupVar name
  case op of
    Nothing -> emit $ "mov " ++ regName lhs ++ ", " ++ regName rhs
    _ -> error "uh oh, I don't know that operator :("
genStmt (Ret e _) = do
  r <- genExpr e
  emit $ "mov " ++ regName 0 ++ ", " ++ regName r
  emit "ret"

genExpr :: Expr -> CodeGen Register
genExpr (IntExpr n _) = do
  r <- freshReg
  emit $ "mov " ++ regName r ++ ", " ++ show n
  return r
genExpr (Ident name _) = lookupVar name
genExpr (UnExpr op e) = do
  r1 <- genExpr e
  r <- freshReg
  instrName <- case op of
    Neg -> return "neg"
    _ -> error "illegal unary operation"
  emit $ "mov " ++ regName r ++ ", " ++ regName r1
  emit $ instrName ++ " " ++ regName r
  return r
genExpr (BinExpr op e1 e2) = do
  r1 <- genExpr e1
  r2 <- genExpr e2
  r <- freshReg
  instrName <- case op of 
    Add -> return "add"
    _ -> error "unknown binary operation"
  emit $ "mov " ++ regName r ++ ", " ++ regName r1
  emit $ instrName ++ " " ++ regName r ++ ", " ++ regName r2
  return r
