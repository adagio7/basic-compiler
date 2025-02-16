{-# LANGUAGE OverloadedStrings #-}

module Lexer (
    lexme
) where

-- Resolve conflicting imports
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (char, space1, string, alphaNumChar, letterChar)

import Data.Void (Void)
import Data.Functor (($>))
import Token (Token(..))

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String 

-- Space Consumer
sc :: Parser ()
sc = L.space space1 lineComment blockComment
    where
        lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

pKeyword :: Parser Token
pKeyword = do
    kw <- choice (map string ["if", "else", "while", "func", "return", "->"]) <* notFollowedBy alphaNumChar
    return $ TokKeyword kw

pNull :: Parser Token
pNull = string "null" $> TokNull

pComma :: Parser Token
pComma = char ',' *> sc *> return TokComma

pColon :: Parser Token
pColon = char ':' *> sc *> return TokColon

pDot :: Parser Token
pDot = char '.' *> sc *> return TokDot

pBool :: Parser Token
pBool = choice
    [
        string "True" $> TokBool True
        , string "False" $> TokBool False
    ]

pInt :: Parser Token
pInt = do
    num <- L.signed sc L.decimal
    notFollowedBy (char '.' <|> alphaNumChar)
    return $ TokInt num

pFloat :: Parser Token
pFloat = do
    num <- L.signed sc L.float
    -- No more remaining digits or decimal points
    notFollowedBy (char '.' <|> alphaNumChar)
    return $ TokFloat num

pString :: Parser Token 
pString = do
    -- Consume the opening quote
    _ <- char '"'
    str <- manyTill L.charLiteral (char '"')
    return $ TokString str 

-- All ops should be in descending length order
-- As the operators are greedily matched
pOp :: Parser Token
pOp = TokOp <$> choice (map (try . string) 
    ["&&", "||", "==", "!=", "<=", ">=", "<", ">", "+", "-", "*", "/", "="])

-- Identifier should not start with a number, can contain alphanum or underscores
pIdent :: Parser Token
pIdent = do
    firstChar <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let ident = firstChar : rest
    return $ TokIdent ident

pLParen :: Parser Token
pLParen = char '(' *> sc *> return TokLParen

pRParen :: Parser Token
pRParen = char ')' *> sc *> return TokRParen

pLBrack :: Parser Token
pLBrack = char '[' *> sc *> return TokLBrack

pRBrack :: Parser Token
pRBrack = char ']' *> sc *> return TokRBrack

pLBrace :: Parser Token
pLBrace = char '{' *> sc *> return TokLBrace

pRBrace :: Parser Token
pRBrace = char '}' *> sc *> return TokRBrace

pToken :: Parser Token
pToken = choice
    -- The ordering of this matters, as it's greedily matched
    [
        try pKeyword        -- Placed before pString, as keyword can be parsed as string
        , try pNull         -- Placed before pString, same as above
        , try pComma
        , try pColon
        , try pDot
        , try pBool
        , try pInt
        , try pFloat
        , try pIdent        -- Placed after pInt, as int can be parsed as ident
        , try pString
        , try pOp
        , try pLParen, try pRParen
        , try pLBrack, try pRBrack
        , try pLBrace, try pRBrace
    ]

lexme :: Parser Token
lexme = sc *> pToken <* sc

