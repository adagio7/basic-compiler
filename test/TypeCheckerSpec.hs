module TypeCheckerSpec (
    spec
) where

import Test.Hspec
import Debug.Trace

import AST
import TypeChecker

import qualified Data.Map as Map

spec :: Spec
spec = do
    let typeEnv = Map.empty

    describe "should type check base data types" $ do
	it "should type check integers" $ do
	    let result = inferType typeEnv (IntLit 123)
	    result `shouldBe` Right TInt

	it "should type check floats" $ do
	    let result = inferType typeEnv (FloatLit 3.14)
	    result `shouldBe` Right TFloat

	it "should type check strings" $ do
	    let result = inferType typeEnv (StringLit "hello")
	    result `shouldBe` Right TString

	it "should type check booleans" $ do
	    let result = inferType typeEnv (Boolean True)
	    result `shouldBe` Right TBool

	it "should type check null" $ do
	    let result = inferType typeEnv Null
	    result `shouldBe` Right TNull

    describe "should type check variables" $ do
	it "should type check existing variables" $ do
	    let typeEnv = Map.fromList [("x", TInt), ("y", TFloat)]
	    let result = inferType typeEnv (Ident "x")
	    result `shouldBe` Right TInt

	it "should type check undefined variables" $ do
	    let result = inferType typeEnv (Ident "x")
	    result `shouldBe` Left "Undefined variable: x"

    describe "should type check unary operators" $ do
	describe "should type check negation" $ do
	    it "should type check negation of integer" $ do
		let result = inferType typeEnv (UnOp Neg (IntLit 123))
		result `shouldBe` Right TInt

	    it "should type check negation of boolean" $ do
		let result = inferType typeEnv (UnOp Not (Boolean True))
		result `shouldBe` Right TBool

    describe "should type check binary operators" $ do
	describe "should type check numerical operations" $ do
	    it "should type check addition" $ do
		let result = inferType typeEnv (BinOp Add (IntLit 1) (IntLit 2))
		result `shouldBe` Right TInt

	    it "should type check subtraction" $ do
		let result = inferType typeEnv (BinOp Sub (IntLit 1) (IntLit 2))
		result `shouldBe` Right TInt

	    it "should type check multiplication" $ do
		let result = inferType typeEnv (BinOp Mul (FloatLit 1.2) (IntLit 2))
		result `shouldBe` Right TFloat

	    it "should type check division" $ do
		let result = inferType typeEnv (BinOp Div (IntLit 1) (FloatLit 2.31))
		result `shouldBe` Right TFloat
	    
	describe "should type check logical operations" $ do
	    it "should type check and" $ do
		let result = inferType typeEnv (BinOp And (Boolean True) (Boolean False))
		result `shouldBe` Right TBool

	    it "should type check or" $ do
		let result = inferType typeEnv (BinOp Or (Boolean True) (Boolean False))
		result `shouldBe` Right TBool

	describe "should type check comparison operations" $ do
	    it "should type check equality" $ do
		let result = inferType typeEnv (BinOp Eq (IntLit 1) (IntLit 2))
		result `shouldBe` Right TBool

	    it "should type check inequality" $ do
		let result = inferType typeEnv (BinOp Neq (IntLit 1) (IntLit 2))
		result `shouldBe` Right TBool

	    it "should type check less than" $ do
		let result = inferType typeEnv (BinOp Lt (IntLit 1) (IntLit 2))
		result `shouldBe` Right TBool

	    it "should type check greater than" $ do
		let result = inferType typeEnv (BinOp Gt (IntLit 1) (IntLit 2))
		result `shouldBe` Right TBool

	    it "should type check less than or equal" $ do
		let result = inferType typeEnv (BinOp Leq (IntLit 1) (IntLit 2))
		result `shouldBe` Right TBool

	    it "should type check greater than or equal" $ do
		let result = inferType typeEnv (BinOp Geq (IntLit 1) (IntLit 2))
		result `shouldBe` Right TBool

    describe "should type check function" $ do
	-- Note: Functions shouldn't add to type envrionment, as they create a new scope
	it "should type check function with no arguments" $ do
	    let expTypeEnv = Map.fromList []
	    let result = inferType typeEnv (Fun (Ident "foo") [] TNull (IntLit 123))
	    result `shouldBe` Right TNull
	    checkEnvEquality typeEnv expTypeEnv `shouldBe` True

	it "should type check function with arguments" $ do
	    let expTypeEnv = Map.fromList []
	    let result = inferType typeEnv (Fun (Ident "foo") [(Ident "x", TInt), (Ident "y", TFloat)] TNull (IntLit 123))

	    result `shouldBe` Right TNull
	    checkEnvEquality typeEnv expTypeEnv `shouldBe` True

        it "should type check function with return type" $ do
	    let expTypeEnv = Map.fromList []
	    let result = inferType typeEnv (Fun (Ident "foo") [(Ident "x", TInt), (Ident "y", TFloat)] TFloat (Return (FloatLit 123)))
	    result `shouldBe` Right TFloat
	    checkEnvEquality typeEnv expTypeEnv `shouldBe` True


	it "should type check function with existing variables" $ do
	    let typeEnv = Map.fromList [("x", TInt), ("y", TFloat)]
	    let expTypeEnv = Map.fromList [("x", TInt), ("y", TFloat)]
	    let result = inferType typeEnv (Fun (Ident "foo") [(Ident "x", TInt), (Ident "y", TFloat)] TFloat (Return (FloatLit 123)))

	    result `shouldBe` Right TFloat
	    checkEnvEquality typeEnv expTypeEnv `shouldBe` True


checkEnvEquality :: TypeEnv -> TypeEnv -> Bool
checkEnvEquality env1 env2 =
    Map.toList env1 == Map.toList env2

