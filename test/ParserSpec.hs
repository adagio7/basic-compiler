module ParserSpec (
    spec
) where

import Test.Hspec

import Data.Either (isLeft)
import Text.Megaparsec (parse)

import AST
import Token
import Parser

spec :: Spec
spec = do
    describe "Parser Single Token" $ do
        it "should parse integers" $ do
	    let result = parse pExpr "fakeFile" [TokInt 123]
	    result `shouldBe` Right (IntLit 123)

	it "should parse variables" $ do
	    let result = parse pExpr "fakeFile" [TokIdent "x"]
	    result `shouldBe` Right (Ident "x")

	it "should parse empty strings" $ do
	    let result = parse pExpr "fakeFile" [TokString ""]
	    result `shouldBe` Right (StringLit "")

	it "should parse strings" $ do
	  let result = parse pExpr "fakeFile" [TokString "hello"]
	  result `shouldBe` Right (StringLit "hello")

	it "should parse true" $ do
	    let result = parse pExpr "fakeFile" [TokBool True]
	    result `shouldBe` Right (Boolean True)

	it "should parse false" $ do
	    let result = parse pExpr "fakeFile" [TokBool False]
	    result `shouldBe` Right (Boolean False)

	it "should parse null" $ do
	    let result = parse pExpr "fakeFile" [TokNull]
	    result `shouldBe` Right (Null)

    describe "Parser Unary Operators" $ do
	it "should parse negation" $ do
	    let result = parse pExpr "fakeFile" [TokOp "-", TokInt 123]
	    result `shouldBe` Right (UnOp Neg (IntLit 123))

	it "should parse logical not" $ do
	    let result = parse pExpr "fakeFile" [TokOp "!", TokIdent "True"]
	    result `shouldBe` Right (UnOp Not (Ident "True"))
	
    describe "Parser Binary Operators" $ do
	it "should parse addition" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "+", TokInt 2]
	    result `shouldBe` Right (BinOp Add (IntLit 1) (IntLit 2))

	it "should parse subtraction" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "-", TokInt 2]
	    result `shouldBe` Right (BinOp Sub (IntLit 1) (IntLit 2))

	it "should parse multiplication" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "*", TokInt 2]
	    result `shouldBe` Right (BinOp Mul (IntLit 1) (IntLit 2))

	it "should parse division" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "/", TokInt 2]
	    result `shouldBe` Right (BinOp Div (IntLit 1) (IntLit 2))

    describe "Parser Precedence" $ do
	it "should parse multiplication before addition" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "+", TokInt 2, TokOp "*", TokInt 3]
	    result `shouldBe` Right (BinOp Add (IntLit 1) (BinOp Mul (IntLit 2) (IntLit 3)))

	it "should parse division before subtraction" $ do
	    let result = parse pExpr "fakeFile" [TokInt 1, TokOp "-", TokInt 2, TokOp "/", TokInt 3]
	    result `shouldBe` Right (BinOp Sub (IntLit 1) (BinOp Div (IntLit 2) (IntLit 3)))

    describe "Parser Parentheses" $ do
	it "should parse parentheses" $ do
	    let result = parse pExpr "fakeFile" [TokLParen, TokInt 1, TokOp "+", TokInt 2, TokRParen, TokOp "*", TokInt 3]
	    result `shouldBe` Right (BinOp Mul (BinOp Add (IntLit 1) (IntLit 2)) (IntLit 3))

	it "should parse nested parentheses" $ do
	    let result = parse pExpr "fakeFile" [TokLParen, TokInt 1, TokOp "+", TokLParen, TokInt 2, TokOp "*", TokInt 3, TokRParen, TokRParen, TokOp "*", TokInt 4]
	    result `shouldBe` Right (BinOp Mul (BinOp Add (IntLit 1) (BinOp Mul (IntLit 2) (IntLit 3))) (IntLit 4))

    describe "Parser If" $ do
	it "should parse if" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "if", TokIdent "x", TokKeyword "then", TokInt 1, TokKeyword "else", TokInt 2]
	    result `shouldBe` Right (If (Ident "x") (IntLit 1) (IntLit 2))

    describe "Parser Let" $ do
	it "should parse let" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "let", TokIdent "x", TokOp "=", TokInt 1]
	    result `shouldBe` Right (Let (Ident "x") (IntLit 1))

	it "should parse conditional let" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "let", TokIdent "x", TokOp "=", TokKeyword "if", TokIdent "x", TokKeyword "then", TokInt 1, TokKeyword "else", TokInt 2]
	    result `shouldBe` Right (Let (Ident "x") (If (Ident "x") (IntLit 1) (IntLit 2)))

    describe "Parser Function" $ do
	it "should parse function" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokLParen, TokRParen, TokKeyword "->", TokIdent "null", TokLBrace, TokInt 1, TokRBrace]
	    result `shouldBe` Right (Fun (Ident "f") [] TNull (IntLit 1))
	
	it "should parse function with assignment" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokLParen, TokRParen, TokKeyword "->", TokIdent "null", TokLBrace, TokKeyword "let", TokIdent "x", TokOp "=", TokInt 1, TokRBrace]
	    result `shouldBe` Right (Fun (Ident "f") [] TNull (Let (Ident "x") (IntLit 1)))

	it "should parse function with parameter" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokLParen, TokIdent "x", TokColon, TokIdent "int", TokRParen, TokKeyword "->", TokIdent "null", TokLBrace, TokInt 1, TokRBrace]
	    result `shouldBe` Right (Fun (Ident "f") [(Ident "x", TInt)] TNull (IntLit 1))

	it "should parse function with parameters" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokLParen, TokIdent "x", TokColon, TokIdent "int", TokComma, TokIdent "y", TokColon, TokIdent "float", TokRParen, TokKeyword "->", TokIdent "null", TokLBrace, TokInt 1, TokRBrace]
	    result `shouldBe` Right (Fun (Ident "f") [(Ident "x", TInt), (Ident "y", TFloat)] TNull (IntLit 1))

	it "should parse function with int return type" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "func", TokIdent "f", TokLParen, TokRParen, TokKeyword "->", TokIdent "int", TokLBrace, TokInt 1, TokRBrace]
	    result `shouldBe` Right (Fun (Ident "f") [] TInt (IntLit 1))
	
    describe "Parser returns" $ do
	it "should parse return null" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "return", TokNull]
	    result `shouldBe` Right (Return Null)
	
	it "should parse return int " $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "return", TokInt 1]
	    result `shouldBe` Right (Return (IntLit 1))

	it "should parse return string" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "return", TokString "hello"]
	    result `shouldBe` Right (Return (StringLit "hello"))

	it "should parse return boolean" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "return", TokBool True]
	    result `shouldBe` Right (Return (Boolean True))

	it "should fail to parse empty return" $ do
	    let result = parse pExpr "fakeFile" [TokKeyword "return"]
	    result `shouldSatisfy` isLeft

    -- describe "Parser Errors" $ do
	-- TODO: write some test for parser fails
	    
