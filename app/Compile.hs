{-# LANGUAGE OverloadedStrings #-}

module Compile
  ( Job(..)
  , compile
  ) where

import Compile.AAsm (codeGen)
import Control.Monad (unless)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Error (L1ExceptT, generalFail)

import Control.Monad.IO.Class

import System.Process.Typed

data Job = Job
  { src :: FilePath
  , out :: FilePath
  } deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let code = codeGen ast
  let assFile = out job ++ ".s"
  liftIO $ writeFile assFile (unlines code)
  gccExit <- runProcess $ shell ("gcc " ++ assFile ++ " -o " ++ out job)
  unless (gccExit == ExitSuccess) $ generalFail "GCC failed to understand my assembly :(((" 1
