{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Compile.Parser
  ( parseAST
  ) where

import           Compile.AST (AST(..), Expr(..), Op(..), Stmt(..), Simp(..), Ctrl(..), HexOrDecInteger (..), ExprType (..))
import           Error (L1ExceptT, parserFail)

import           Control.Monad.Combinators.Expr
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor (void)
import Data.Text (Text)
import           Data.Void (Void)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.ByteString
import Data.Text.Encoding (decodeUtf8')
import Prelude hiding (init)
import Data.Maybe (isJust)

parseAST :: FilePath -> L1ExceptT AST
parseAST path = do
  text <- liftIO $ Data.ByteString.readFile path
  case decodeUtf8' text of
    Left err -> parserFail $ show err
    Right decoded ->
      case parse astParser path decoded of
        Left err -> parserFail $ errorBundlePretty err
        Right ast -> return ast

type Parser = Parsec Void Text

astParser :: Parser AST
astParser = do
  sc
  -- this parses `int main()` literally, like in the L1 grammar
  reserved "int"
  reserved "main"
  parens $ pure ()
  ast <- braces $ do
    pos <- getSourcePos
    stmts <- many stmt
    return $ Function stmts pos
  void eof
  return ast

stmt :: Parser Stmt
stmt = (Simple <$> stmtSimple) <|> stmtRet <|> stmtBreak <|> stmtContinue <|> stmtBlock <|> stmtIf <|> stmtFor <|> stmtWhile

stmtSimple :: Parser Simp
stmtSimple = do
  s <- decl <|> simp
  semi
  return s

stmtSimple' :: Parser Simp
stmtSimple' = do
  decl <|> simp

stmtBlock :: Parser Stmt
stmtBlock = do
  inner <- braces $ many stmt
  return $ Block inner

stmtIf :: Parser Stmt
stmtIf = do
  reserved "if"
  cond <- parens expr
  ifB <- stmt
  elseT <- optional $ reserved "else"
  elseB <- (if isJust elseT then stmt else pure $ Block [])
  return $ Control $ If cond ifB elseB

stmtFor :: Parser Stmt
stmtFor = do
  reserved "for"
  (init, cond, step) <- parens $ do
    init <- optional stmtSimple
    case init of
      Nothing -> do
        semi
      _ -> pure ()
    cond <- expr
    semi
    step <- optional stmtSimple'
    return (init, cond, step)
  Control . For init cond step <$> stmt

stmtWhile :: Parser Stmt
stmtWhile = do
  reserved "while"
  cond <- parens expr
  Control . While cond <$> stmt

stmtBreak :: Parser Stmt
stmtBreak = do
  reserved "break"
  semi
  return $ Control Break

stmtContinue :: Parser Stmt
stmtContinue = do
  reserved "continue"
  semi
  return $ Control Continue

emptyStmt :: Parser Stmt
emptyStmt = return $ Block []

decl :: Parser Simp
decl = try declInit <|> declNoInit

declNoInit :: Parser Simp
declNoInit = do
  pos <- getSourcePos
  t <- try typ
  name <- identifier
  return $ Decl t name pos

declInit :: Parser Simp
declInit = do
  pos <- getSourcePos
  t <- try typ
  name <- identifier
  void $ symbol "="
  e <- expr
  return $ Init t name e pos

simp :: Parser Simp
simp = do
  pos <- getSourcePos
  name <- identifier
  op <- asnOp
  e <- expr
  return $ Asgn name op e pos

asnOp :: Parser (Maybe Op)
asnOp = do
  op <- operator
  case op of
    "+=" -> pure (Just Add)
    "*=" -> pure (Just Mul)
    "-=" -> pure (Just Sub)
    "/=" -> pure (Just Div)
    "%=" -> pure (Just Mod)
    "&=" -> pure (Just BitwiseAnd)
    "^=" -> pure (Just BitwiseXor)
    "|=" -> pure (Just BitwiseOr)
    "<<=" -> pure (Just LeftShift)
    ">>=" -> pure (Just RightShift)
    "=" -> pure Nothing
    x -> fail $ "Nonexistent assignment operator: " ++ x
  <?> "assignment operator"

stmtRet :: Parser Stmt
stmtRet = do
  pos <- getSourcePos
  reserved "return"
  e <- expr
  semi
  return $ Control $ Ret e pos

expr' :: Parser Expr
expr' = parens expr <|> intExpr <|> boolLit1 <|> boolLit2 <|> identExpr <|> unExpr

intExpr :: Parser Expr
intExpr = do
  pos <- getSourcePos
  val <- number
  return $ IntExpr val pos

-- TODO refactor
boolLit1 :: Parser Expr
boolLit1 = do
  reserved "true"
  return $ BoolLit True

boolLit2 :: Parser Expr
boolLit2 = do
  reserved "false"
  return $ BoolLit False

unExpr :: Parser Expr
unExpr = do
  void $ symbol "-"
  UnExpr Neg <$> expr'

identExpr :: Parser Expr
identExpr = do
  pos <- getSourcePos
  name <- identifier
  return $ Ident name pos

opTable :: [[Operator Parser Expr]]
opTable =
  [ [ Prefix (UnExpr Neg <$ symbol "-")
    , Prefix (UnExpr LogicalNot <$ symbol "!")
    , Prefix (UnExpr BitwiseNot <$ symbol "~")
    ]
  , [ InfixL (BinExpr Mul <$ symbol "*")
    , InfixL (BinExpr Div <$ symbol "/")
    , InfixL (BinExpr Mod <$ symbol "%")
    ]
  , [ InfixL (BinExpr Add <$ symbol "+")
    , InfixL (BinExpr Sub <$ symbol "-")
    ]
  , [ InfixL (BinExpr LeftShift <$ symbol "<<")
    , InfixL (BinExpr RightShift <$ symbol ">>")
    ]
  , [ InfixL (BinExpr IntLt <$ symbol "<")
    , InfixL (BinExpr IntLe <$ symbol "<=")
    , InfixL (BinExpr IntGt <$ symbol ">")
    , InfixL (BinExpr IntGe <$ symbol ">=")
    ]
  , [ InfixL (BinExpr Equals <$ symbol "==")
    , InfixL (BinExpr EqualsNot <$ symbol "!=")
    ]
  , [ InfixL (BinExpr BitwiseAnd <$ symbol "&")
    ]
  , [ InfixL (BinExpr BitwiseXor <$ symbol "^")
    ]
  , [ InfixL (BinExpr BitwiseOr <$ symbol "|")
    ]
  , [ InfixL (BinExpr LogicalAnd <$ symbol "&&")
    ]
  , [ InfixL (BinExpr LogicalOr <$ symbol "||")
    ]
  -- The odds of this crap parsing ternary expressions correctly are low.
  , [ InfixR (BinExpr Ternary1 <$ symbol "?")
    , InfixR (BinExpr Ternary2 <$ symbol ":")
    ]
  ]

expr :: Parser Expr
expr = try (makeExprParser expr' opTable) <?> "expression"

-- Lexer starts here, probably worth moving to its own file at some point
sc :: Parser ()
sc = L.space mySpace lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockCommentNested "/*" "*/"

mySpace :: (MonadParsec e s m, Token s ~ Char) => m ()
mySpace = void $ some (oneOf ['\t', '\r', '\n', ' '])

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semi :: Parser ()
semi = void $ symbol ";"

number :: Parser HexOrDecInteger
number = try hexadecimal <|> decimal <|> decimal0 <?> "number"

decimal :: Parser HexOrDecInteger
decimal = do
  void $ lookAhead $ oneOf digits1
  val <- lexeme L.decimal
  return $ Dec val

digits1 :: [Char]
digits1 = "123456789"

decimal0 :: Parser HexOrDecInteger
decimal0 = do
  void $ char '0'
  void sc
  return $ Dec 0

hexadecimal :: Parser HexOrDecInteger
hexadecimal = do
  void $ char '0'
  void $ oneOf ['x','X']
  val <- lexeme L.hexadecimal
  --if val > 2^(31 :: Integer) then fail "integer out of bounds" else pure val
  --if val > 0xffffffff then fail "integer out of bounds" else pure val
  return $ Hex val

reserved :: Text -> Parser ()
reserved w = void (lexeme $ try (string w <* notFollowedBy identLetter))

typ :: Parser ExprType
typ = do
  word <- identifierRaw
  case word of
    "int" -> return IntT
    "bool" -> return BoolT
    w -> fail $ "Nonexistent type: " ++ w

reservedWords :: [String]
reservedWords =
  [ "alloc"
  , "alloc_array"
  , "assert"
  , "bool"
  , "break"
  , "char"
  , "continue"
  , "else"
  , "false"
  , "for"
  , "if"
  , "int"
  , "NULL"
  , "print"
  , "read"
  , "return"
  , "string"
  , "struct"
  , "true"
  , "void"
  , "while"
  ]

-- Operations
opStart :: Parser Char
opStart = oneOf [ '=', '+', '-', '*', '/', '%', '&', '^', '|', '<', '>', '!', '~' ]

opLetter :: Parser Char
opLetter = oneOf [ '=', '&', '|', '<', '>' ]

operator :: Parser String
operator = lexeme ((:) <$> opStart <*> many opLetter)

-- Identifiers
identStart :: Parser Char
identStart = oneOf alpha <|> char '_'

identLetter :: Parser Char
identLetter = oneOf alphaNums <|> char '_'

alpha :: [Char]
alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

alphaNums :: [Char]
alphaNums = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

identifier :: Parser String
identifier = (lexeme . try) (p >>= check) <|> parens identifier
  where
    p = (:) <$> identStart <*> many identLetter
    check x =
      if x `elem` reservedWords
        then fail (x ++ " is reserved")
        else return x

identifierRaw :: Parser String
identifierRaw = (lexeme . try) (p >>= check) <|> parens identifier
  where
    p = (:) <$> identStart <*> many identLetter
    check = return

