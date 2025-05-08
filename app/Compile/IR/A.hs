module Compile.IR.A
    ( A
    , Function(..)
    , Inst(..)
    , RegOrMem(..)
    , Register
    , Memory
    , eax
    , edx
    , ebp
    , esp
    , xmm0
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
  | Add RegOrMem RegOrMem
  | Sub RegOrMem RegOrMem
  | Mul RegOrMem RegOrMem
  | Div RegOrMem
  | Neg RegOrMem
  | Cdq
  | Leave
  | Return
  | Zero RegOrMem
  | Comment String
  deriving (Show)

data RegOrMem
  = Reg Register
  | Mem Memory
  | Imm Int32
  deriving (Eq, Ord, Show)

eax = Reg 0
edx = Reg 3
ebp = Reg 6
esp = Reg 7
xmm0 = Reg 16

type Register = Integer
type Memory = Integer
