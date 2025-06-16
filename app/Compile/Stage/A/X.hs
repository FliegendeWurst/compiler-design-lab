{-# LANGUAGE LambdaCase #-}

module Compile.Stage.A.X
  ( fromXToA
  ) where


import           Control.Monad.State
import qualified Data.Map as Map

import Data.Maybe (listToMaybe)
import Data.Foldable (forM_)
import Compile.IR.A (A, Function, Inst (..), Register, RegOrMem (..), xmm0, eax, edx)
import Compile.IR.Y (Expr (..), LitOrIdent (..))
import Compile.IR.X (X, Stmt (Asgn, Decl) )
import qualified Compile.IR.X as X
import qualified Compile.IR.A as A
import Data.Functor (void)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (when)
import qualified Compile.IR.Y as Y
import qualified Compile.IR.Z as Z


type VarName = String

type AAsmAlloc = Map VarName RegOrMem

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: AAsmAlloc
  , usedRegs :: Set Register
  , freeStackSlots :: Set Integer
  , nextStackSlot :: Integer
  , nextExpr :: Integer
  , code :: [Inst]
  , constPropagation :: Bool
  , maxStackUse :: Integer
  }
  deriving (Show)

fromXToA :: X -> Bool -> A
fromXToA funs constProp = map (`function'` constProp) funs

function' :: X.Function -> Bool -> Function
function' (X.Function name stmts) constProp = do
  let initialState = CodeGenState Map.empty Set.empty Set.empty 0 0 [] constProp 0
  let r = execState (genBlock stmts) initialState
  A.Function name (maxStackUse r) $ code r

regsToAllocate :: [Register]
regsToAllocate = [1,2,4,5,8,9,10,11,12,13,14,15]

nextReg :: CodeGen (Maybe Register)
nextReg = do
  curr <- get
  let mapX = usedRegs curr
  let freeRegs = filter (`Set.notMember` mapX) regsToAllocate
  return $ listToMaybe freeRegs

spillSomeRegs :: CodeGen ()
spillSomeRegs = do
  -- just spill all for now
  curr <- get
  let mapPrevious = regMap curr
  -- emit $ Comment $ "spilling " ++ show mapPrevious
  forM_ (Map.assocs mapPrevious) (\case
    (name, Reg y) -> do
      newLoc <- spillReg y
      modify $ \s -> s { regMap = Map.insert name newLoc (regMap s) }
      modify $ \s -> s { usedRegs = Set.delete y (usedRegs s) }
    _ -> return ()
    )

spillReg :: Register -> CodeGen RegOrMem
spillReg x = do
  ss <- getFreeStackSlot
  emit $ Mov (Mem ss) (Reg x)
  return $ Mem ss

getFreeStackSlot :: CodeGen Integer
getFreeStackSlot = do
  free <- gets freeStackSlots
  case Set.lookupMin free of
    Nothing -> do
      ss <- gets nextStackSlot
      modify $ \s -> s { nextStackSlot = ss + 1 }
      modify $ \s -> s { maxStackUse = max (maxStackUse s) (nextStackSlot s) }
      return ss
    Just it -> do
      modify $ \s -> s { freeStackSlots = Set.delete it free }
      return it

freshReg :: CodeGen Register
freshReg = do
  r <- nextReg
  case r of
    Nothing -> do
      spillSomeRegs
      freshReg
    Just reg -> do
      when (reg > 15) (error "incorrect register used")
      return reg

assignVar :: VarName -> RegOrMem -> CodeGen ()
assignVar name r = do
  curr <- gets regMap
  case Map.lookup name curr of
    Just (Reg y) -> modify $ \s -> s { usedRegs = Set.delete y (usedRegs s) }
    _ -> return ()
  modify $ \s -> s {regMap = Map.insert name r (regMap s)}
  case r of
    Reg y -> modify $ \s -> s { usedRegs = Set.insert y (usedRegs s) }
    _ -> return ()
  --curr <- get
  --if 2 == unsafePerformIO (do
  --  print $ show curr
  --  return 1) then error "failure" else return ()

lookupVar :: VarName -> CodeGen RegOrMem
lookupVar name = do
  m <- gets regMap
  case Map.lookup name m of
    Just r -> return r
    Nothing -> error $ "unreachable, fix your semantic analysis I guess " ++ name ++ " map = " ++ show m

reloadVar :: VarName -> CodeGen RegOrMem
reloadVar name = do
  m <- gets regMap
  case Map.lookup name m of
    Just (Reg r) -> return $ Reg r
    Just (Mem x) -> do
      newReg <- reload (Mem x)
      modify $ \s -> s { regMap = Map.insert name (Reg newReg) (regMap s) }
      modify $ \s -> s { usedRegs = Set.insert newReg (usedRegs s) }
      modify $ \s -> s { freeStackSlots = Set.insert x (freeStackSlots s) }
      return $ Reg newReg
    Just (Imm i) -> do
      return $ Imm i
      --newReg <- freshReg
      --emit $ Mov (Reg newReg) (Imm i)
      --modify $ \s -> s {regMap = Map.insert name (Reg newReg) (regMap s)}
      --modify $ \s -> s { usedRegs = Set.insert newReg (usedRegs s) }
      --return newReg
    Nothing -> error $ "failed to reload var " ++ name

discardVar :: VarName -> CodeGen ()
discardVar name = do
  m <- gets regMap
  case Map.lookup name m of
    Just (Reg r) -> do
      modify $ \s -> s { usedRegs = Set.delete r (usedRegs s) }
    Just (Mem x) -> do
      modify $ \s -> s { freeStackSlots = Set.insert x (freeStackSlots s) }
    Just (Imm _) -> return ()
    Nothing -> return () -- odd
  modify $ \s -> s { regMap = Map.delete name m }

reloadVarForWrite :: VarName -> CodeGen Register
reloadVarForWrite name = do
  m <- gets regMap
  case Map.lookup name m of
    Just (Reg r) -> return r
    Just (Mem x) -> do
      newReg <- reload (Mem x)
      modify $ \s -> s { regMap = Map.insert name (Reg newReg) (regMap s) }
      modify $ \s -> s { usedRegs = Set.insert newReg (usedRegs s) }
      modify $ \s -> s { freeStackSlots = Set.insert x (freeStackSlots s) }
      return newReg
    Just (Imm _) -> do
      newReg <- freshReg
      modify $ \s -> s {regMap = Map.insert name (Reg newReg) (regMap s)}
      modify $ \s -> s { usedRegs = Set.insert newReg (usedRegs s) }
      return newReg
    Nothing -> error "unreachable, fix your semantic analysis I guess"

reload :: RegOrMem -> CodeGen Register
reload (Reg x) = return x
reload (Mem x) = do
  emit $ Mov xmm0 (Mem x)
  reg <- freshReg
  emit $ Mov (Reg reg) xmm0
  return reg
reload (Imm i) = do
  newReg <- freshReg
  emit $ Mov (Reg newReg) (Imm i)
  return newReg

emit :: Inst -> CodeGen ()
emit instr = modify $ \s -> s {code = code s ++ [instr]}

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> CodeGen ()
genStmt (Decl t name) = do
  emit $ Comment $ "decl " ++ show name ++ " typ " ++ show t
  r <- freshReg
  assignVar name $ Reg r
genStmt (X.Discard name) = do
  discardVar name
genStmt (Asgn name (Plain (Lit n))) = do
  emit $ Comment $ "asgn literal " ++ show name ++ " " ++ show n
  assignVar name $ Imm n
genStmt (Asgn name (Plain (Ident n))) = do
  emit $ Comment $ "asgn " ++ show name ++ " = " ++ show n
  rhs <- reloadVar n
  constP <- gets constPropagation
  case (constP, rhs) of
    (True, Imm i) -> assignVar name $ Imm i
    _ -> do
      lhs <- reloadVarForWrite name
      emit $ Mov (Reg lhs) rhs
genStmt (Asgn name (UnExpr op (Lit n))) = do
  emit $ Comment $ "asgn to unary op " ++ show name ++ " = " ++ show op ++ show n
  assignVar name (Imm $ -n)
genStmt (Asgn name (UnExpr op (Ident n))) = do
  emit $ Comment $ "asgn to unary op " ++ show name ++ " = " ++ show op ++ show n
  rhs <- reloadVar n
  r <- reloadVarForWrite name
  void $ case op of
    Z.Neg -> return ()
  emit $ Mov (Reg r) rhs
  emit $ A.Neg (Reg r)
genStmt (Asgn name (BinExpr op e1 e2)) = do
  emit $ Comment $ "asgn to binary op " ++ show name ++ " = " ++ show op ++ " " ++ show e1 ++ " " ++ show e2
  r1 <- genPlainExpr e1
  r2 <- genPlainExpr e2
  constP <- gets constPropagation
  case (constP, r1, r2, op) of
    (True, Imm i1, Imm i2, o) | (o /= Z.Div && o /= Z.Mod) || i2 > 0 || i2 < -1 -> do
      case op of
        Z.Add -> do
          assignVar name (Imm $ i1 + i2)
        Z.Sub ->
          assignVar name (Imm $ i1 - i2)
        Z.Mul ->
          assignVar name (Imm $ i1 * i2)
        Z.Div ->
          assignVar name (Imm $ i1 `quot` i2)
        Z.Mod ->
          assignVar name (Imm $ i1 `rem` i2)
      return ()
    _ -> do
      reg <- reloadVarForWrite name
      let r = Reg reg
      case op of
        Z.Add -> do
          emit $ Mov r r1
          emit $ A.Add r r2
        Z.Sub -> do
          emit $ Mov r r1
          emit $ A.Sub r r2
        Z.Mul -> do
          emit $ Mov r r1
          emit $ A.Mul r r2
        Z.Div -> do
          emit $ Mov eax r1
          emit Cdq
          r2InRegister <- case r2 of
            Imm _ -> do
              newReg <- freshReg
              emit $ Mov (Reg newReg) r2
              return (Reg newReg)
            Reg register -> return $ Reg register
            Mem x -> return $ Mem x
          emit $ A.Div r2InRegister
          emit $ Mov r eax
        Z.Mod -> do
          emit $ Mov eax r1
          emit Cdq

          r2InRegister <- case r2 of
            Imm _ -> do
              newReg <- freshReg
              emit $ Mov (Reg newReg) r2
              return (Reg newReg)
            Reg register -> return $ Reg register
            Mem x -> return $ Mem x

          emit $ A.Div r2InRegister
          emit $ Mov r edx
genStmt (X.Ret (Lit n)) = do
  emit $ Mov eax (Imm n)
  emit Leave
  emit Return
genStmt (X.Ret (Ident n)) = do
  rhs <- reloadVar n
  emit $ Mov eax rhs
  emit Leave
  emit Return

genPlainExpr :: Y.LitOrIdent -> CodeGen RegOrMem
genPlainExpr (Lit n) = return $ Imm n
genPlainExpr (Ident name) = lookupVar name
