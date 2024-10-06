module AST (
    Expr(..),
    UOp(..),
    BOp(..)
) where

data Expr = 
    Var String
    | StringLit String
    | Num Int
    | BinOp BOp Expr Expr
    | UnOp UOp Expr 
    | If Expr Expr Expr
    | Let String Expr
    | Fun String Expr
    deriving (Show, Eq)

-- Unary Operators
data UOp = 
    Neg | Not
    deriving (Show, Eq)

-- Binary Operators
data BOp =
    -- Arithmetic Operators
    Add | Sub | Mul | Div |
    -- Logical Operators
    And | Or | 
    -- Comparison Operators
    Eq | Neq | Lt | Gt | Leq | Geq
    deriving (Show, Eq)
