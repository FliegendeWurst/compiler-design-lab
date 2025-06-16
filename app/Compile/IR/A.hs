module Compile.IR.A
    ( A
    , Function(..)
    , Inst(..)
    , Label
    , RegOrMem(..)
    , Register
    , Memory
    , eax
    , eax'
    , edx
    , ebp
    , esp
    , xmm0
    , asMem
    ) where
import Data.Int (Int32)

type A = [Function]

data Function = Function
    { name :: String
    , stack :: Integer
    , body :: [Inst]
    }
    deriving (Show)

data Inst
  = Mov RegOrMem RegOrMem
  | Xor RegOrMem RegOrMem
  | And RegOrMem RegOrMem
  | Or RegOrMem RegOrMem
  | Add RegOrMem RegOrMem
  | Sub RegOrMem RegOrMem
  | Mul RegOrMem RegOrMem
  | Div RegOrMem
  | Neg RegOrMem
  | Not RegOrMem
  | Cdq
  | Leave
  | Return
  | Zero RegOrMem
  -- Requires at least one operand in a register
  | Cmp RegOrMem RegOrMem
  | Lbl Label
  | Jump Label
  | Je Label
  | Jne Label
  | Jgt Label
  | Jge Label
  | Jle Label
  | Jlt Label
  | Jz Label
  | Jnz Label
  -- Require first operand to be register
  | CmovE RegOrMem RegOrMem
  | CmovNe RegOrMem RegOrMem
  | CmovGt RegOrMem RegOrMem
  | CmovGe RegOrMem RegOrMem
  | CmovLe RegOrMem RegOrMem
  | CmovLt RegOrMem RegOrMem
  | Comment String
  deriving (Show)

type Label = String

data RegOrMem
  = Reg Register
  | Mem Memory
  | Imm Int32
  deriving (Eq, Ord, Show)

asMem :: RegOrMem -> Memory
asMem (Mem x) = x
asMem x = error $ "this is not memory: " ++ show x

eax = Reg 0
eax' = 0
edx = Reg 3
ebp = Reg 6
esp = Reg 7
xmm0 = Reg 16

type Register = Integer
type Memory = Integer
