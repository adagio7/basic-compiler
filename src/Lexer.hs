{-# LANGUAGE OverloadedStrings #-}

module Lexer (
    lexme
) where

-- Resolve conflicting imports
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (char, space1, string, alphaNumChar)

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
    kw <- choice (map string ["if", "else", "while", "func"]) <* notFollowedBy alphaNumChar
    return $ TokKeyword kw

pNull :: Parser Token
pNull = string "null" $> TokNull

pBool :: Parser Token
pBool = choice
    [
        string "True" $> TokBool True
        , string "False" $> TokBool False
    ]

pInt :: Parser Token
pInt = TokInt <$> L.signed sc L.decimal

pFloat :: Parser Token
pFloat = TokFloat <$> L.signed sc L.float

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
    [
        -- The ordering of this matters, as the first one that matches will be returned
        try pKeyword
        , try pNull
        , try pBool
        , try pInt
        , try pFloat
        , try pString
        , try pOp
        , try pLParen
        , try pRParen
        , try pLBrack
        , try pRBrack
        , try pLBrace
        , try pRBrace
    ]

lexme :: Parser Token
lexme = sc *> pToken <* sc

