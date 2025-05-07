{-# LANGUAGE OverloadedStrings #-}

module Compile
  ( Job(..)
  , compile
  ) where

import Compile.Assembler (assemble)
import Control.Monad (unless)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Error (L1ExceptT, generalFail)

import Control.Monad.IO.Class

import System.Process.Typed
import Compile.Stage.Z.AST (fromASTToZ)
import Compile.Stage.Y.Z (fromZToY)
import Compile.Stage.X.Y (fromYToX)
import Compile.Stage.A.X (fromXToA)

data Job = Job
  { src :: FilePath
  , out :: FilePath
  } deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let z = fromASTToZ ast
  let y = fromZToY z
  let x = fromYToX y
  -- liftIO $ print x
  let a = fromXToA x
  -- let aFile = out job ++ ".a"
  -- liftIO $ writeFile aFile (unlines $ concatMap (\x -> (map (\y -> show y) (body x))) a)
  let code = assemble a
  let assFile = out job ++ ".s"
  liftIO $ writeFile assFile (unlines code)
  gccExit <- runProcess $ shell ("gcc " ++ assFile ++ " -o " ++ out job)
  unless (gccExit == ExitSuccess) $ generalFail "GCC failed to understand my assembly :(((" 1
