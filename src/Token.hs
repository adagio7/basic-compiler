module Token (
    Token(..)
) where

data Token
    = TokInt Int                -- Integer literals 42
    | TokString String          -- String literals "hello"
    | TokKeyword String         -- Keywords like "if", "else", "while"
    | TokIdent String           -- Identifiers like "x", "y", "z"
    | TokOp String              -- Operators like "+", "-", "*", "/", "&&", "||"
    | TokLParen | TokRParen    -- Parentheses
    | TokLBrace | TokRBrace    -- Braces
    | TokNewline
    | TokEOF
    deriving (Show, Eq)
