module ParserSpec (
    spec
) where

import Test.Hspec

import Text.Megaparsec (parse)

import AST
import Token
import Parser

spec :: Spec
spec = do
    describe "Parser Single Token" $ do
        it "should parse integers" $ do
	    let result = parse pExpr "fakeFile" [TokInt 123]
	    result `shouldBe` Right (Num 123)

	it "should parse variables" $ do
	    let result = parse pExpr "fakeFile" [TokIdent "x"]
	    result `shouldBe` Right (Var "x")

	-- it "should parse strings" $ do
	--   let result = parse pExpr "fakeFile" [TokString "hello"]
	--   result `shouldBe` Right (StringLit "hello")

    describe "Parser Unary Operators" $ do
	it "should parse negation" $ do
	    let result = parse pExpr "fakeFile" [TokOp "-", TokInt 123]
	    result `shouldBe` Right (UnOp Neg (Num 123))

	it "should parse logical not" $ do
	    let result = parse pExpr "fakeFile" [TokOp "!", TokIdent "True"]
	    result `shouldBe` Right (UnOp Not (Var "True"))
	
    describe "Parser Binary Operators" $ do
	it "should parse addition" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "+", TokInt 2]
	    result `shouldBe` Right (BinOp Add (Num 1) (Num 2))

	it "should parse subtraction" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "-", TokInt 2]
	    result `shouldBe` Right (BinOp Sub (Num 1) (Num 2))

	it "should parse multiplication" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "*", TokInt 2]
	    result `shouldBe` Right (BinOp Mul (Num 1) (Num 2))

	it "should parse division" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "/", TokInt 2]
	    result `shouldBe` Right (BinOp Div (Num 1) (Num 2))

    describe "Parser Precedence" $ do
	it "should parse multiplication before addition" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "+", TokInt 2, TokOp "*", TokInt 3]
	    result `shouldBe` Right (BinOp Add (Num 1) (BinOp Mul (Num 2) (Num 3)))

	it "should parse division before subtraction" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "-", TokInt 2, TokOp "/", TokInt 3]
	    result `shouldBe` Right (BinOp Sub (Num 1) (BinOp Div (Num 2) (Num 3)))

 --    describe "Parser Parentheses" $ do
	-- it "should parse parentheses" $ do
	--     let result = parse pExpr "fakeFile" [TokLParen, TokInt 1, TokOp "+", TokInt 2, TokRParen, TokOp "*", TokInt 3]
	--     result `shouldBe` Right (BinOp Mul (BinOp Add (Num 1) (Num 2)) (Num 3))

    describe "Parser If" $ do
	it "should parse if" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "if", TokIdent "x", TokKeyword "then", TokInt 1, TokKeyword "else", TokInt 2]
	    result `shouldBe` Right (If (Var "x") (Num 1) (Num 2))

    describe "Parser Let" $ do
	it "should parse let" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "let", TokIdent "x", TokOp "=", TokInt 1]
	    result `shouldBe` Right (Let "x" (Num 1))

    describe "Parser Function" $ do
	it "should parse function" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokInt 1]
	    result `shouldBe` Right (Fun "f" (Num 1))

	it "should parse function with assignment" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokKeyword "let", TokIdent "x", TokOp "=", TokInt 1]
	    result `shouldBe` Right (Fun "f" (Let "x" (Num 1)))
	
	it "should parse function with if" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokKeyword "if", TokIdent "x", TokKeyword "then", TokInt 1, TokKeyword "else", TokInt 2]
	    result `shouldBe` Right (Fun "f" (If (Var "x") (Num 1) (Num 2)))

	it "should parse function with function" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokKeyword "func", TokIdent "g", TokInt 1]
	    result `shouldBe` Right (Fun "f" (Fun "g" (Num 1)))

	it "should parse function with function with assignment" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokKeyword "func", TokIdent "g", TokKeyword "let", TokIdent "x", TokOp "=", TokInt 1]
	    result `shouldBe` Right (Fun "f" (Fun "g" (Let "x" (Num 1))))

	it "should parse function with assignment and if" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokKeyword "let", TokIdent "x", TokOp "=", TokKeyword "if", TokIdent "x", TokKeyword "then", TokInt 1, TokKeyword "else", TokInt 2]
	    result `shouldBe` Right (Fun "f" (Let "x" (If (Var "x") (Num 1) (Num 2))))

    

