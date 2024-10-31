module AST (
    TypeEnv,
    Expr(..),
    Type(..),
    UOp(..),
    BOp(..)
) where

import qualified Data.Map as Map

type TypeEnv = Map.Map String Type

data Expr = 
    Ident String
    | StringLit String
    | Boolean Bool
    | Null
    | IntLit Int
    | FloatLit Float
    | BinOp BOp Expr Expr
    | UnOp UOp Expr 
    | If Expr Expr Expr
    | Let Expr Expr
    | Fun Expr [(Expr, Type)] Type Expr  -- Function Name, Arguments, Return Type, Body
    | Return Expr
    deriving (Show, Eq)

-- Base Data Types
data Type =
    TInt | TFloat | TBool | TString | TNull
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
