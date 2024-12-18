module TypeChecker (
    inferType
) where

import AST

import qualified Data.Map as Map

inferType :: TypeEnv -> Expr -> Either String Type
inferType env expr = case expr of
    IntLit _ -> Right TInt
    FloatLit _ -> Right TFloat
    Boolean _ -> Right TBool
    StringLit _ -> Right TString
    Null -> Right TNull

    Ident str -> case Map.lookup str env of
        Just t -> Right t
        Nothing -> Left $ "Undefined variable: " ++ str

    UnOp op e -> do
        t <- inferType env e

        case op of
            Not | t == TBool -> Right TBool
                | otherwise  -> Left "Type mismatch in unary operation !: expected TBool"

            Neg | t == TInt || t == TFloat -> Right t
                | otherwise -> Left "Type mismatch in unary operation -: expected TInt or TFloat"

            _ -> Left $ "Unknown unary operator: " ++ show op

    BinOp op e1 e2 -> do
        t1 <- inferType env e1
        t2 <- inferType env e2
        case op of
            -- Numerical Operations
            Add -> checkNumericOp "+" t1 t2 
            Sub -> checkNumericOp "-" t1 t2
            Mul -> checkNumericOp "*" t1 t2
            Div -> checkNumericOp "/" t1 t2
            
            -- Logical Operations
            And -> checkBoolOp "&&" t1 t2
            Or -> checkBoolOp "||" t1 t2

            -- Comparison Operations
            Eq  -> checkComparisonOp "==" t1 t2
            Neq -> checkComparisonOp "!=" t1 t2
            Lt  -> checkComparisonOp "<" t1 t2
            Gt  -> checkComparisonOp ">" t1 t2
            Leq -> checkComparisonOp "<=" t1 t2
            Geq -> checkComparisonOp ">=" t1 t2

    -- TODO: If condition
    If cond e1 e2 -> do
        -- Note that we don't check the type of the branches, just that they are defined
        tcond <- inferType env cond
        if tcond == TBool then do
            -- Ensure that branches are defined
            _ <- inferType env e1
            _ <- inferType env e2
            Right TNull
        else
            Left "Condition in if statement must be boolean"

    Fun (Ident name) args retType body -> do
        -- Insert the params to new env
        -- Handles global access & variable shadowing (as new replaces old)
        let env' = foldr(\(Ident arg, t) acc -> Map.insert arg t acc) env args

        -- trace ("Function " ++ name ++ " with args " ++ show args ++ " and return type " ++ show retType)

        -- Check if the body type matches the return type
        checkReturnType env' retType body

    _ -> Right TNull


-- Checks if the type of the expression is numeric
checkNumericOp :: String -> Type -> Type -> Either String Type
checkNumericOp op t1 t2 
    | t1 == TInt && t2 == TInt = Right TInt
    | t1 == TInt && t2 == TFloat = Right TFloat
    | t1 == TFloat && t2 == TInt = Right TFloat
    | t1 == TFloat && t2 == TFloat = Right TFloat
    | otherwise = Left $ "Type mismatch in numeric operation" ++ op ++ ": " ++ show t1 ++ " and " ++ show t2

-- Checks if the type of the expression is boolean
checkBoolOp :: String -> Type -> Type -> Either String Type
checkBoolOp op t1 t2
    | t1 == TBool && t2 == TBool = Right TBool
    | otherwise = Left $ "Type mismatch in boolean operation" ++ op ++ ": " ++ show t1 ++ " and " ++ show t2
    ++ show t2

checkComparisonOp :: String -> Type -> Type -> Either String Type
checkComparisonOp op t1 t2
    | t1 == t2 = Right TBool
    | otherwise = Left $ "Type mismatch in comparison operation" ++ op ++ ": " ++ show t1 ++ " and " ++ show t2
    ++ show t2

checkReturnType :: TypeEnv -> Type -> Expr -> Either String Type
checkReturnType env expectedType body = case body of
    Return e -> do
        t <- inferType env e
        if t == expectedType then
            Right expectedType
        else
            Left "Return type does not match function return type"

    -- TODO: check if branhces
    -- Check every branch
    -- If cond thenBranch elseBranch -> do
    --     tthen <- checkReturnType env expectedType thenBranch
    --     telse <- checkReturnType env expectedType elseBranch

    -- No return implicilty returns null
    _ -> Right TNull
