{-# LANGUAGE LambdaCase #-}

module Compile.Stage.A.X
  ( fromXToA
  ) where


import           Control.Monad.State
import qualified Data.Map as Map

import Data.Maybe (listToMaybe)
import Data.Foldable (forM_)
import Compile.IR.A (A, Function, Inst (..), Register, RegOrMem (..), xmm0, eax, edx, Label, eax', ecx)
import Compile.IR.Y (Expr (..), LitOrIdent (..))
import Compile.IR.X (X, Stmt (..) )
import qualified Compile.IR.X as X
import qualified Compile.IR.A as A
import Data.Functor (void)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (when)
import qualified Compile.IR.Y as Y
import qualified Compile.IR.Z as Z
import Data.Int (Int32)
import Prelude hiding (init)
import Util (snd3, fst3, thd3, expect)


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
  , nextLabel :: Integer
  -- always maps to (Mem x)
  , varStore :: Map VarName RegOrMem
  -- stores for each loop: label for continue, break
  , loopStack :: [(Label,Label, Label)]
  }
  deriving (Show)

fromXToA :: X -> Bool -> A
fromXToA funs constProp = map (`function'` constProp) funs

function' :: X.Function -> Bool -> Function
function' (X.Function name stmts) constProp = do
  let initialState = CodeGenState Map.empty Set.empty Set.empty 0 0 [] constProp 0 0 Map.empty []
  let r = execState (genBlock stmts) initialState
  A.Function name (maxStackUse r) $ code r

regsToAllocate :: [Register]
regsToAllocate = [1,2,4,5,8,9,10,11,12,13,14,15]

-- body, continue, break
pushLoop :: Label -> Label -> Label -> CodeGen ()
pushLoop bodyL contL breakL = do
  modify $ \s -> s { loopStack = (bodyL,contL,breakL):loopStack s }
popLoop :: CodeGen ()
popLoop = do
  modify $ \s -> s { loopStack = tail (loopStack s) }

peekLoop :: CodeGen (Label, Label, Label)
peekLoop = do
  ls <- gets loopStack
  return $ head ls

nextReg :: CodeGen (Maybe Register)
nextReg = do
  curr <- get
  let inUse = usedRegs curr
  let freeRegs = filter (`Set.notMember` inUse) regsToAllocate
  return $ listToMaybe freeRegs

spillSomeRegs :: CodeGen ()
spillSomeRegs = do
  -- just spill all for now
  curr <- get
  let mapPrevious = regMap curr
  let storePrevious = varStore curr
  -- emit $ Comment $ "spilling " ++ show mapPrevious
  forM_ (Map.assocs mapPrevious) (\case
    (_, Mem _) -> return () -- assumed correct already
    (name, regOrImm) -> do
      let prevLoc = Map.lookup name storePrevious
      case prevLoc of
        Just loc -> do
          emit $ Mov loc regOrImm
          modify $ \s -> s { regMap = Map.insert name loc (regMap s) }
        Nothing -> do
          newLoc <- spillReg regOrImm
          modify $ \s -> s { varStore = Map.insert name newLoc (varStore s) }
          modify $ \s -> s { regMap = Map.insert name newLoc (regMap s) }
      case regOrImm of
        Reg y -> do
          modify $ \s -> s { usedRegs = Set.delete y (usedRegs s) }
        _ -> pure ()
    )

spillReg :: RegOrMem -> CodeGen RegOrMem
spillReg x = do
  ss <- getFreeStackSlot
  emit $ Mov (Mem ss) x
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

freshLabel :: CodeGen Label
freshLabel = do
  x <- gets nextLabel
  modify $ \s -> s { nextLabel = x + 1 }
  return $ "lbl" ++ show x

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
    Nothing -> do
      m2 <- gets varStore
      case Map.lookup name m2 of
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
      -- modify $ \s -> s { freeStackSlots = Set.insert x (freeStackSlots s) }
      return $ Reg newReg
    Just (Imm i) -> do
      return $ Imm i
      --newReg <- freshReg
      --emit $ Mov (Reg newReg) (Imm i)
      --modify $ \s -> s {regMap = Map.insert name (Reg newReg) (regMap s)}
      --modify $ \s -> s { usedRegs = Set.insert newReg (usedRegs s) }
      --return newReg
    Nothing -> do
      m2 <- gets varStore
      let realLoc = expect ("failed to reload stored variable " ++ name ++ " in " ++ show m2 ++ " or normal map " ++ show m) $ Map.lookup name m2
      modify $ \s -> s { regMap = Map.insert name realLoc (regMap s) }
      reloadVar name

discardVar :: VarName -> CodeGen ()
discardVar name = do
  m <- gets regMap
  let loc = expect "impossible?" $ Map.lookup name m
  m2 <- gets varStore
  let locPermanent = Map.lookup name m2
  case locPermanent of
    Nothing -> do
      newLoc <- spillReg loc
      modify $ \s -> s { varStore = Map.insert name newLoc (varStore s) }
      modify $ \s -> s { regMap = Map.insert name newLoc (regMap s) }
    Just mem -> do
      when (loc /= mem) $ do
        emit $ Mov mem loc
  m' <- gets regMap
  let loc' = Map.lookup name m'
  case loc' of
    Just (Reg r) -> do
      modify $ \s -> s { usedRegs = Set.delete r (usedRegs s) }
    Just (Mem _) -> do
      -- modify $ \s -> s { freeStackSlots = Set.insert x (freeStackSlots s) }
      pure () -- permanently stored
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
    Nothing -> do
      m2 <- gets varStore
      let realLoc = expect ("failed to find stored variable " ++ name ++ " in " ++ show m2 ++ " or normal map " ++ show m) $ Map.lookup name m2
      modify $ \s -> s { regMap = Map.insert name realLoc (regMap s) }
      reloadVarForWrite name


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

reloadInto :: Register -> RegOrMem -> CodeGen ()
reloadInto dest src = do
  emit $ Mov (Reg dest) src

emit :: Inst -> CodeGen ()
emit instr = modify $ \s -> s {code = code s ++ [instr]}

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

boolToInt :: Bool -> Int32
boolToInt False = 0
boolToInt True = 1

genStmt :: Stmt -> CodeGen ()
genStmt (If cond ifB elseB) = do
  elseL <- freshLabel
  commonL <- freshLabel
  cond' <- case cond of
    Ident x -> reloadVar x
    LitB x -> pure $ Imm (boolToInt x)
    Lit _ -> error "tried to reload integer as condition"
  cond'' <- reload cond'
  emit $ Cmp (Reg cond'') (Imm (boolToInt True))
  spillSomeRegs
  emit $ Jne elseL
  genStmt ifB
  spillSomeRegs
  emit $ Comment "jump after if"
  emit $ Jump commonL
  emit $ Lbl elseL
  genStmt elseB
  spillSomeRegs
  -- implicit: Jump commonL
  emit $ Lbl commonL
genStmt (For init body) = do
  bodyL <- freshLabel
  continueL <- freshLabel
  breakL <- freshLabel
  pushLoop bodyL continueL breakL
  forM_ init genStmt
  spillSomeRegs
  emit $ Lbl bodyL
  forM_ body genStmt
  emit $ Lbl breakL
  popLoop
genStmt ForStepLabel = do
  spillSomeRegs
  lbls <- peekLoop
  emit $ Lbl (snd3 lbls)
genStmt Continue = do
  spillSomeRegs
  lbls <- peekLoop
  emit $ Comment "continue"
  emit $ Jump (fst3 lbls) -- remember, this continue is only issued after the step check
genStmt Break = do
  spillSomeRegs
  lbls <- peekLoop
  emit $ Comment "break"
  emit $ Jump (thd3 lbls)
genStmt (Block inner) = do
  forM_ inner genStmt
  -- FIXME: spill
genStmt (Decl t name) = do
  emit $ Comment $ "decl " ++ show name ++ " typ " ++ show t
  r <- freshReg
  assignVar name $ Reg r
genStmt (X.Discard name) = do
  discardVar name -- FIXME probably broken due to loops
genStmt (Asgn name (Plain (Lit n))) = do
  emit $ Comment $ "asgn literal " ++ show name ++ " " ++ show n
  assignVar name $ Imm n
genStmt (Asgn name (Plain (LitB n))) = do
  emit $ Comment $ "asgn literal B " ++ show name ++ " " ++ show n
  assignVar name $ Imm (boolToInt n)
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
genStmt (Asgn name (UnExpr op (LitB n))) = do
  emit $ Comment $ "asgn to unary op " ++ show name ++ " = " ++ show op ++ show n
  assignVar name (Imm $ (boolToInt $ not n))
genStmt (Asgn name (UnExpr op (Ident n))) = do
  emit $ Comment $ "asgn to unary op " ++ show name ++ " = " ++ show op ++ show n
  rhs <- reloadVar n
  r <- reloadVarForWrite name
  emit $ Mov (Reg r) rhs
  void $ case op of
    Z.Neg -> do
      emit $ A.Neg (Reg r)
    Z.LogicalNot -> do
      emit $ A.Xor (Reg r) (Imm 1)
    Z.BitwiseNot -> do
      emit $ A.Not (Reg r)
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
        _ -> error "TODO: const prop for more"
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
        Z.IntGe -> do
          emit $ Zero r
          emit $ Mov edx (Imm 1)
          reloadInto eax' r1
          emit $ Cmp eax r2
          emit $ CmovGe r edx
        Z.IntGt -> do
          emit $ Zero r
          emit $ Mov edx (Imm 1)
          reloadInto eax' r1
          emit $ Cmp eax r2
          emit $ CmovGt r edx
        Z.IntLe -> do
          emit $ Zero r
          emit $ Mov edx (Imm 1)
          reloadInto eax' r1
          emit $ Cmp eax r2
          emit $ CmovLe r edx
        Z.IntLt -> do
          emit $ Zero r
          emit $ Mov edx (Imm 1)
          reloadInto eax' r1
          emit $ Cmp eax r2
          emit $ CmovLt r edx
        Z.Equals -> do
          emit $ Zero r
          emit $ Mov edx (Imm 1)
          reloadInto eax' r1
          emit $ Cmp eax r2
          emit $ CmovE r edx
        Z.EqualsNot -> do
          emit $ Zero r
          emit $ Mov edx (Imm 1)
          reloadInto eax' r1
          emit $ Cmp eax r2
          emit $ CmovNe r edx
        Z.LogicalAnd -> do
          emit $ Mov r r1
          emit $ And r r2
        Z.LogicalOr -> do
          emit $ Mov r r1
          emit $ Or r r2
        Z.LeftShift -> do
          emit $ Mov xmm0 ecx
          emit $ Mov r r1
          emit $ Mov ecx r2
          emit $ SalCl r
          emit $ Mov ecx xmm0
        Z.RightShift -> do
          emit $ Mov xmm0 ecx
          emit $ Mov r r1
          emit $ Mov ecx r2
          emit $ SarCl r
          emit $ Mov ecx xmm0
        Z.BitwiseAnd -> do
          emit $ Mov r r1
          emit $ And r r2
        Z.BitwiseOr -> do
          emit $ Mov r r1
          emit $ Or r r2
        Z.BitwiseXor -> do
          emit $ Mov r r1
          emit $ Xor r r2
        Z.Ternary1 -> do
          error "why is there still a ternary here..."
        Z.Ternary2 -> do
          error "why is there still a ternary here..."
genStmt (X.Ret (Lit n)) = do
  emit $ Mov eax (Imm n)
  emit Leave
  emit Return
genStmt (X.Ret (LitB _)) = error "tried to return boolean"
genStmt (X.Ret (Ident n)) = do
  rhs <- reloadVar n
  emit $ Mov eax rhs
  emit Leave
  emit Return

genPlainExpr :: Y.LitOrIdent -> CodeGen RegOrMem
genPlainExpr (Lit n) = return $ Imm n
genPlainExpr (LitB n) = return $ Imm (boolToInt n)
genPlainExpr (Ident name) = lookupVar name
