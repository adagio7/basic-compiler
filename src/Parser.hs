module Parser (
    pExpr
) where

import Data.Void (Void)
import Data.Functor (($>))
import Text.Megaparsec (Parsec, try, choice, token, label, ErrorItem(Label), sepBy)
import Control.Monad.Combinators.Expr (
    makeExprParser,
    Operator (..))

import Token
import AST (Expr(..), Type(..), UOp(..), BOp(..))

import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE

type Parser = Parsec Void [Token]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm table

pTerm :: Parser Expr
pTerm = choice
    [
        try pIdent
        , try pFloat
        , try pInt
        , try pString
        , try pBool
        , try pNull
        , try pIf
        , try pLet
        , try pFun
        , try pReturn
        -- Consume parentheses
        , try (pLParen *> pExpr <* pRParen)
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

pInt :: Parser Expr
pInt = do
    n <- token testInt (setErrorLabel "integer")
    return $ IntLit n

    where
        testInt (TokInt n) = Just n
        testInt _ = Nothing

pFloat :: Parser Expr
pFloat = do
    n <- token testFloat (setErrorLabel "float")
    return $ FloatLit n

    where
        testFloat (TokFloat n) = Just n
        testFloat _ = Nothing

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

pReturn :: Parser Expr
pReturn = do
    _ <- pKeyword "return"
    expr <- pExpr
    return $ Return expr

pIf :: Parser Expr
pIf = do
    _ <- pKeyword "if"
    cond <- pExpr
    _ <- pKeyword "then"
    thenExpr <- pExpr
    _ <- pKeyword "else"
    elseExpr <- pExpr
    return $ If cond thenExpr elseExpr

pIdent :: Parser Expr
pIdent = do 
    str <- token testIdent (setErrorLabel "identifier")
    return $ Ident str 
  where
    testIdent (TokIdent ident) = Just ident
    testIdent _ = Nothing

pBool :: Parser Expr
pBool = do
    b <- token testBool (setErrorLabel "boolean")
    return $ Boolean b

    where
        testBool (TokBool b) = Just b
        testBool _ = Nothing

pNull :: Parser Expr
pNull = do
    _ <- token testNull (setErrorLabel "null")
    return Null

    where
        testNull TokNull = Just TokNull
        testNull _ = Nothing

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
    args <- pTypedParams
    _ <- pKeyword "->"
    retType <- pType
    _ <- pLBrace
    body <- pExpr
    _ <- pRBrace
    return $ Fun name args retType body

pTypedParams :: Parser [(Expr, Type)]
pTypedParams = do
    _ <- pLParen
    params <- pTypedParam `sepBy` pComma
    _ <- pRParen
    return params

pTypedParam :: Parser (Expr, Type)
pTypedParam = do
    name <- pIdent
    _ <- pColon
    ty <- pType
    return (name, ty)

pOp :: String -> Parser Token
pOp op = token testOp (setErrorLabel ("operator " ++ op))
  where
    testOp (TokOp op') | op' == op = Just (TokOp op')
    testOp _ = Nothing

pLParen :: Parser Token
pLParen = token testLParen (setErrorLabel "left parenthesis")
  where
    testLParen TokLParen = Just TokLParen
    testLParen _ = Nothing

pRParen :: Parser Token
pRParen = token testRParen (setErrorLabel "right parenthesis")
  where
    testRParen TokRParen = Just TokRParen
    testRParen _ = Nothing

pLBrack :: Parser Token
pLBrack = token testLBrack (setErrorLabel "left bracket")
  where
    testLBrack TokLBrack = Just TokLBrack
    testLBrack _ = Nothing

pRBrack :: Parser Token
pRBrack = token testRBrack (setErrorLabel "right bracket")
  where
    testRBrack TokRBrack = Just TokRBrack
    testRBrack _ = Nothing

pLBrace :: Parser Token
pLBrace = token testLBrace (setErrorLabel "left brace")
  where
    testLBrace TokLBrace = Just TokLBrace
    testLBrace _ = Nothing

pRBrace :: Parser Token
pRBrace = token testRBrace (setErrorLabel "right brace")
  where
    testRBrace TokRBrace = Just TokRBrace
    testRBrace _ = Nothing

pColon :: Parser Token
pColon = token testColon (setErrorLabel "colon")
  where
    testColon TokColon = Just TokColon
    testColon _ = Nothing

pComma :: Parser Token
pComma = token testComma (setErrorLabel "comma")
  where
    testComma TokComma = Just TokComma
    testComma _ = Nothing

pType :: Parser Type
pType = token testType (setErrorLabel "type")
  where
    testType (TokIdent "int") = Just TInt
    testType (TokIdent "float") = Just TFloat
    testType (TokIdent "bool") = Just TBool
    testType (TokIdent "string") = Just TString
    testType (TokIdent "null") = Just TNull
    testType _ = Nothing

-- Util function to set error label
setErrorLabel :: String -> Set.Set (ErrorItem (Token))
setErrorLabel msg = Set.singleton $ Label (NE.fromList(msg))
