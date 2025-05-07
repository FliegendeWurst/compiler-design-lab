{-# LANGUAGE QuasiQuotes #-}

module Compile.Assembler
    ( assemble
    ) where
import Compile.IR.A (A, Function (..), RegOrMem ( .. ), Inst (..), xmm0)

import TH (litFile)
import Numeric (showHex)

assemble :: A -> [String]
assemble funs = globalPrelude ++ concatMap function' funs

globalPrelude :: [String]
globalPrelude = lines [litFile|app/Compile/assembly_template.s|]

function' :: Function -> [String]
function' fun = [
    displayName (name fun) ++ ":",
    "enter " ++ show (stack fun * 4) ++ ", 0"
    ] ++
    map inst' (body fun) ++
    [ "" ]

inst' :: Inst -> String
inst' (Mov dest src) | dest == xmm0 = "movd " ++ loc' xmm0 ++ ", " ++ loc' src
inst' (Mov dest src) | src == xmm0 = "movd " ++ loc' dest ++ ", " ++ loc' xmm0
inst' (Mov dest src) = "mov " ++ loc' dest ++ ", " ++ loc' src
inst' (Add dest src) = "add " ++ loc' dest ++ ", " ++ loc' src
inst' (Sub dest src) = "sub " ++ loc' dest ++ ", " ++ loc' src
inst' (Mul dest (Imm val)) = "imul " ++ loc' dest ++ ", " ++ loc' dest ++ ", " ++ loc' (Imm val)
inst' (Mul dest src) = "imul " ++ loc' dest ++ ", " ++ loc' src
inst' (Div src) = "idiv " ++ loc' src
inst' (Neg dest) = "neg " ++ loc' dest
inst' Cdq = "cdq"
inst' Leave = "leave"
inst' Return = "ret"
inst' (Zero dest) = "xor " ++ loc' dest ++ ", " ++ loc' dest
inst' (Comment s) = "# " ++ s

loc' :: RegOrMem -> String
loc' (Reg 0) = "eax" -- not allocated
loc' (Reg 1) = "ebx"
loc' (Reg 2) = "ecx"
loc' (Reg 3) = "edx" -- not allocated
loc' (Reg 4) = "esi"
loc' (Reg 5) = "edi"
loc' (Reg 6) = "ebp" -- not allocated
loc' (Reg 7) = "esp" -- not allocated
loc' (Reg 8) = "r8d"
loc' (Reg 9) = "r9d"
loc' (Reg 10) = "r10d"
loc' (Reg 11) = "r11d"
loc' (Reg 12) = "r12d"
loc' (Reg 13) = "r13d"
loc' (Reg 14) = "r14d"
loc' (Reg 15) = "r15d"
loc' (Reg 16) = "xmm0"
loc' (Reg x) = error $ "unknown register " ++ show x
loc' (Mem x) = "DWORD PTR [rsp + " ++ show (x * 4) ++ "]"
loc' (Imm n) = "0x" ++ showHex n ""

displayName :: String -> String
displayName "main" = "_main"
displayName x = x
