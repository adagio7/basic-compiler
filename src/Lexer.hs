{-# LANGUAGE OverloadedStrings #-}

module Lexer (
    lexme
) where

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (char, space1, newline)

import Data.Void (Void)
import Token (Token(..))

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String 

sc :: Parser ()
sc = L.space space1 empty empty

-- pKeyword :: Parser Token
-- pKeyword = do
--     kw <- choice (map string ["if", "else", "while", "func"]) <* notFollowedBy alphaNumChar
--     return $ TokKeyword kw

pInt :: Parser Token
pInt = TokInt <$> L.signed sc L.decimal

pString :: Parser Token 
pString = do
    -- Consume the opening quote
    _ <- char '"'
    str <- manyTill L.charLiteral (char '"')
    return $ TokString str 

-- pOp :: Parser Token
-- pOp = TokOp <$> choice (map string ["+", "-", "*", "/", "&&", "||", "==", "!=", "<", ">", "<=", ">="])

pLParen :: Parser Token
pLParen = char '(' *> sc *> return TokLParen


pRParen :: Parser Token
pRParen = char ')' *> sc *> return TokRParen

pLBrace :: Parser Token
pLBrace = char '{' *> sc *> return TokLBrace

pRBrace :: Parser Token
pRBrace = char '}' *> sc *> return TokRBrace

pNewline :: Parser Token
pNewline = newline *> sc *> return TokNewline

pEOF :: Parser Token
pEOF = eof *> return TokEOF

pToken :: Parser Token
pToken = choice
    [
        -- try pKeyword,
        try pInt,
        try pString,
        -- try pOp,
        try pLParen,
        try pRParen,
        try pLBrace,
        try pRBrace,
        try pNewline,
        try pEOF
    ]

lexme :: Parser Token
lexme = sc *> pToken
