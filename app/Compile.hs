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
  , constProp :: Bool
  } deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  liftIO $ print "AST parsed.."
  liftIO $ print ast
  semanticAnalysis ast
  let z = fromASTToZ ast
  liftIO $ print "Z-IR lowering done.."
  liftIO $ print z
  let y = fromZToY z
  liftIO $ print "Y-IR lowering done.."
  liftIO $ print y
  let x = fromYToX y
  liftIO $ print "X-IR lowering done.."
  liftIO $ print x
  let a = fromXToA x (constProp job)
  liftIO $ print "A-IR lowering done.."
  -- let aFile = out job ++ ".a"
  -- liftIO $ writeFile aFile (unlines $ concatMap (\x -> (map (\y -> show y) (body x))) a)
  let code = assemble a
  liftIO $ print "Assembly lowering done.."
  let assFile = out job ++ ".s"
  liftIO $ writeFile assFile (unlines code)
  gccExit <- runProcess $ shell ("gcc " ++ assFile ++ " -o " ++ out job)
  liftIO $ print "GCC done.."
  unless (gccExit == ExitSuccess) $ generalFail "GCC failed to understand my assembly :(((" 1
