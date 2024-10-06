module Parser (
    pExpr
) where

import Data.Void (Void)
import Data.Functor (($>))
import Text.Megaparsec (Parsec, try, choice, token, label, ErrorItem(Label))
import Control.Monad.Combinators.Expr (
    makeExprParser,
    Operator (..))

import Token
import AST (Expr(..), UOp(..), BOp(..))

import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE

type Parser = Parsec Void [Token]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm table

pTerm :: Parser Expr
pTerm = choice
    [
        try pVar
        , try pNum
        , try pIf
        , try pLet
        , try pFun
        -- try pParen
    ]

table :: [[Operator Parser Expr]]
table = 
    [
        [
            Prefix (UnOp Neg <$ pOp "-"),
            Prefix (UnOp Not <$ pOp "!")
        ],
        [
            InfixL (BinOp Mul <$ pOp "*"),
            InfixL (BinOp Div <$ pOp "/")
        ],
        [
            InfixL (BinOp Add <$ pOp "+"),
            InfixL (BinOp Sub <$ pOp "-")
        ]
    ]

pNum :: Parser Expr
pNum = do
    n <- token testNum (setErrorLabel "integer")
    return $ Num n

    where
        testNum (TokInt n) = Just n
        testNum _ = Nothing

pVar :: Parser Expr
pVar = do
    name <- token testVar (setErrorLabel "variable")
    return $ Var name

    where
        testVar (TokIdent name) = Just name
        testVar _ = Nothing

pString :: Parser Expr
pString = do
    str <- token testStr (setErrorLabel "string")
    return $ StringLit str

    where
        testStr (TokString str) = Just str
        testStr _ = Nothing

pKeyword :: String -> Parser Token
pKeyword kw = token testKw (setErrorLabel ("keyword " ++ kw))
  where
    testKw (TokKeyword kw') | kw' == kw = Just (TokKeyword kw')
    testKw _ = Nothing

pIf :: Parser Expr
pIf = do
    _ <- pKeyword "if"
    cond <- pExpr
    _ <- pKeyword "then"
    thenExpr <- pExpr
    _ <- pKeyword "else"
    elseExpr <- pExpr
    return $ If cond thenExpr elseExpr

pIdent :: Parser String
pIdent = token testIdent (setErrorLabel "identifier")
  where
    testIdent (TokIdent ident) = Just ident
    testIdent _ = Nothing

pLet :: Parser Expr
pLet = do
    _ <- pKeyword "let"
    name <- pIdent
    _ <- pOp "="
    val <- pExpr
    return $ Let name val

pFun :: Parser Expr
pFun = do
    _ <- pKeyword "func"
    name <- pIdent
    body <- pExpr
    return $ Fun name body
--
pOp :: String -> Parser Token
pOp op = token testOp (setErrorLabel ("binary operator " ++ op))
  where
    testOp (TokOp op') | op' == op = Just (TokOp op')
    testOp _ = Nothing

-- Util function to set error label
setErrorLabel :: String -> Set.Set (ErrorItem (Token))
setErrorLabel msg = Set.singleton $ Label (NE.fromList(msg))
