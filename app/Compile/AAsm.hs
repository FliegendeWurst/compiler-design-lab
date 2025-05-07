{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Compile.AAsm
  ( codeGen
  ) where

import           Compile.AST (AST(..), Expr(..), Stmt(..), Op (..), intValue)

import           Control.Monad.State
import qualified Data.Map as Map

import TH (litFile)
import Data.Maybe (listToMaybe)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Foldable (forM_)

data RegisterOrSpilled
  = Reg Register
  | Spilled StackSlot
  deriving (Eq, Ord)

type StackSlot = Integer
type Register = Integer

type VarName = String

type AAsmAlloc = Bimap VarName RegisterOrSpilled
type RegLastUsed = Map.Map Register Integer

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: AAsmAlloc
  , regLru :: RegLastUsed -- TODO
  , nextStackSlot :: Integer
  , nextExpr :: Integer
  , code :: [String]
  }

assemblyTemplate :: String
assemblyTemplate = [litFile|app/Compile/assembly_template.s|]

codeGen :: AST -> [String]
codeGen (Block stmts _) = lines assemblyTemplate ++ code (execState (genBlock stmts) initialState)
  where
    initialState = CodeGenState Bimap.empty Map.empty 0 0 []

regName :: Register -> String
regName 0 = "eax" -- not allocated
regName 1 = "ebx"
regName 2 = "ecx"
regName 3 = "edx" -- not allocated
regName 4 = "esi"
regName 5 = "edi"
regName 6 = "ebp" -- not allocated
regName 7 = "esp" -- not allocated
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

regsToAllocate :: [Register]
regsToAllocate = [1,2,4,5,8,9,10,11,12,13,14,15]

realize :: RegisterOrSpilled -> String
realize (Reg x) = regName x
realize (Spilled x) = loadStack x

nextReg :: CodeGen (Maybe Register)
nextReg = do
  curr <- get
  let mapX = regMap curr
  let freeRegs = filter (\x -> Bimap.notMemberR (Reg x) mapX) regsToAllocate
  return $ listToMaybe freeRegs

spillSomeRegs :: CodeGen ()
spillSomeRegs = do
  -- just spill all for now
  curr <- get
  let mapPrevious = regMap curr
  forM_ (Bimap.assocs mapPrevious) (\case
    (name, Reg y) -> do
      newLoc <- spillReg y
      modify $ \s -> s { regMap = Bimap.insert name newLoc (regMap s) }
    _ -> return ()
    )

spillReg :: Register -> CodeGen RegisterOrSpilled
spillReg x = do
  curr <- get
  let ss = nextStackSlot curr
  emit $ "mov " ++ loadStack ss ++ ", " ++ regName x
  modify $ \s -> s {nextStackSlot = ss + 1}
  return $ Spilled ss

freshReg :: CodeGen Register
freshReg = do
  r <- nextReg
  case r of
    Nothing -> do
      spillSomeRegs
      freshReg
    Just reg -> return reg

assignVar :: VarName -> RegisterOrSpilled -> CodeGen ()
assignVar name r = do
  modify $ \s -> s {regMap = Bimap.insert name r (regMap s)}

lookupVar :: VarName -> CodeGen RegisterOrSpilled
lookupVar name = do
  m <- gets regMap
  case Bimap.lookup name m of
    Just r -> return r
    Nothing -> error "unreachable, fix your semantic analysis I guess"

loadStack :: Integer -> String
loadStack x = "DWORD PTR [rsp + " ++ show (4*x) ++ "]"

reloadVar :: VarName -> CodeGen Register
reloadVar name = do
  m <- gets regMap
  case Bimap.lookup name m of
    Just (Reg r) -> return r
    Just (Spilled x) -> do
      newReg <- reload (Spilled x)
      modify $ \s -> s {regMap = Bimap.insert name (Reg newReg) (regMap s)}
      return newReg
    Nothing -> error "unreachable, fix your semantic analysis I guess"

reload :: RegisterOrSpilled -> CodeGen Register
reload (Reg x) = return x
reload (Spilled x) = do
  emit $ "movd xmm0, " ++ loadStack x
  reg <- freshReg
  emit $ "movd " ++ regName reg ++ ", xmm0"
  return reg

emit :: String -> CodeGen ()
emit instr = modify $ \s -> s {code = code s ++ [instr]}

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> CodeGen ()
genStmt (Decl name _) = do
  emit $ "# decl " ++ show name
  r <- freshReg
  assignVar name $ Reg r
genStmt (Init name e _) = do
  emit $ "# init " ++ show name ++ " " ++ show e
  r <- genExpr e
  assignVar name r
genStmt (Asgn name op e src) = do
  emit $ "# asgn " ++ show name ++ " " ++ show op ++ " " ++ show e
  case op of
    Nothing -> do
      rhs <- genExpr e
      lhs <- reloadVar name
      emit $ "mov " ++ regName lhs ++ ", " ++ realize rhs
    Just it -> genStmt (Asgn name Nothing (BinExpr it (Ident name src) e) src)
genStmt (Ret e _) = do
  r <- genExpr e
  emit $ "mov " ++ regName 0 ++ ", " ++ realize r
  emit "leave"
  emit "ret"

genExpr :: Expr -> CodeGen RegisterOrSpilled
genExpr (IntExpr n _) = do
  r <- freshReg
  modify $ \s -> s { regMap = Bimap.insert ("expr__Internal_" ++ show (nextExpr s)) (Reg r) (regMap s) }
  modify $ \s -> s { nextExpr = nextExpr s + 1 }

  emit $ "mov " ++ regName r ++ ", " ++ show (intValue n)
  return $ Reg r
genExpr (Ident name _) = lookupVar name
genExpr (UnExpr op e) = do
  r1 <- genExpr e
  r <- freshReg
  modify $ \s -> s { regMap = Bimap.insert ("expr__Internal_" ++ show (nextExpr s)) (Reg r) (regMap s) }
  modify $ \s -> s { nextExpr = nextExpr s + 1 }

  instrName <- case op of
    Neg -> return "neg"
    _ -> error "illegal unary operation"
  emit $ "mov " ++ regName r ++ ", " ++ realize r1
  emit $ instrName ++ " " ++ regName r
  return $ Reg r
genExpr (BinExpr op e1 e2) = do
  r1 <- genExpr e1
  r2 <- genExpr e2
  r <- freshReg
  modify $ \s -> s { regMap = Bimap.insert ("expr__Internal_" ++ show (nextExpr s)) (Reg r) (regMap s) }
  modify $ \s -> s { nextExpr = nextExpr s + 1 }

  instrName <- case op of
    Add -> return "add"
    Sub -> return "sub"
    Mul -> return "mul"
    Div -> return "div"
    Mod -> return "mod"
    _ -> error "unknown binary operation"
  case op of
    Mul -> do
      emit $ "mov eax," ++ realize r1
      emit $ "mul " ++ realize r2
      emit $ "mov " ++ regName r ++ ",eax"
    Div -> do
      -- emit "xor edx,edx"
      emit $ "mov eax," ++ realize r1
      emit "cdq"
      emit $ "idiv " ++ realize r2
      emit $ "mov " ++ regName r ++ ",eax"
    Mod -> do
      -- emit "xor edx,edx"
      emit $ "mov eax," ++ realize r1
      emit "cdq"
      emit $ "idiv " ++ realize r2
      emit $ "mov " ++ regName r ++ ",edx"
    _ -> do
      emit $ "mov " ++ regName r ++ ", " ++ realize r1
      emit $ instrName ++ " " ++ regName r ++ ", " ++ realize r2
  return $ Reg r
