module Token (
    Token(..)
) where

data Token
    = TokNull                   -- Null token
    | TokInt Int                -- Integer literals 42
    | TokFloat Float            -- Float literals 3.14
    | TokBool Bool              -- Boolean literals True, False
    | TokString String          -- String literals "hello"
    | TokKeyword String         -- Keywords like "if", "else", "while"
    | TokIdent String           -- Identifiers like "x", "y", "z"
    | TokOp String              -- Operators like "+", "-", "*", "/", "&&", "||"
    | TokLParen | TokRParen    -- Parentheses
    | TokLBrack | TokRBrack    -- Brackets
    | TokLBrace | TokRBrace    -- Braces
    deriving (Show, Eq, Ord)
