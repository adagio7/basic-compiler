module LexerSpec (
    spec
)where

-- Avoid conflicting import
import Text.Megaparsec hiding (Token)

import Test.Hspec

import Lexer
import Token

spec :: Spec
spec = do
    describe "Lexer Single Token" $ do
        it "should lex integers" $ do
	    let result = parse lexme "fakeFile" "123"
            result `shouldBe` Right (TokInt 123)
	
	it "should lex char" $ do
	    let result = parse lexme "fakeFile" "\"a\""
	    result `shouldBe` Right (TokString "a")

	it "should lex strings" $ do
	    let result = parse lexme "fakeFile" "\"hello\""
	    result `shouldBe` Right (TokString "hello")

	it "should lex left parentheses" $ do
	    let result = parse lexme "fakeFile" "("
	    result `shouldBe` Right TokLParen

	it "should lex right parentheses" $ do
	    let result = parse lexme "fakeFile" ")"
	    result `shouldBe` Right TokRParen

	it "should lex left braces" $ do
	    let result = parse lexme "fakeFile" "{"
	    result `shouldBe` Right TokLBrace

	it "should lex right braces" $ do
	    let result = parse lexme "fakeFile" "}"
	    result `shouldBe` Right TokRBrace

	it "should lex if" $ do
	    let result = parse lexme "fakeFile" "if"
	    result `shouldBe` Right (TokKeyword "if")

	it "should lex else" $ do
	    let result = parse lexme "fakeFile" "else"
	    result `shouldBe` Right (TokKeyword "else")

	it "should lex while" $ do
	    let result = parse lexme "fakeFile" "while"
	    result `shouldBe` Right (TokKeyword "while")

	it "should lex func" $ do
	    let result = parse lexme "fakeFile" "func"
	    result `shouldBe` Right (TokKeyword "func")

	it "should lex +" $ do
	    let result = parse lexme "fakeFile" "+"
	    result `shouldBe` Right (TokOp "+")

	it "should lex -" $ do
	    let result = parse lexme "fakeFile" "-"
	    result `shouldBe` Right (TokOp "-")

	it "should lex *" $ do
	    let result = parse lexme "fakeFile" "*"
	    result `shouldBe` Right (TokOp "*")
	
	it "should lex /" $ do
	    let result = parse lexme "fakeFile" "/"
	    result `shouldBe` Right (TokOp "/")

	it "should lex &&" $ do
	    let result = parse lexme "fakeFile" "&&"
	    result `shouldBe` Right (TokOp "&&")
	    
	it "should lex ||" $ do
	    let result = parse lexme "fakeFile" "||"
	    result `shouldBe` Right (TokOp "||")

	it "should lex ==" $ do
	    let result = parse lexme "fakeFile" "=="
	    result `shouldBe` Right (TokOp "==")
	    
	it "should lex !=" $ do
	    let result = parse lexme "fakeFile" "!="
	    result `shouldBe` Right (TokOp "!=")
	    
	it "should lex <" $ do
	    let result = parse lexme "fakeFile" "<"
	    result `shouldBe` Right (TokOp "<")

	it "should lex >" $ do
	    let result = parse lexme "fakeFile" ">"
	    result `shouldBe` Right (TokOp ">")

	it "should lex <=" $ do
	    let result = parse lexme "fakeFile" "<="
	    result `shouldBe` Right (TokOp "<=")
	    
	it "should lex >=" $ do
	    let result = parse lexme "fakeFile" ">="
	    result `shouldBe` Right (TokOp ">=")

	it "should lex =" $ do
	    let result = parse lexme "fakeFile" "="
	    result `shouldBe` Right (TokOp "=")
	
    describe "Lexer Multiple Tokens" $ do
	it "should lex multiple ints" $ do
	    let result = parse (some lexme) "fakeFile" "123 456 789"
	    result `shouldBe` Right [TokInt 123, TokInt 456, TokInt 789]

	it "should lex multiple ops" $ do
	    let result = parse (some lexme) "fakeFile" "+-*/"
	    result `shouldBe` Right [TokOp "+", TokOp "-", TokOp "*", TokOp "/"]

	it "should lex multiple tokens" $ do
	    let result = parse (some lexme) "fakeFile" "123 \"hello\" if"
	    result `shouldBe` Right [TokInt 123, TokString "hello", TokKeyword "if"]

	it "should lex multiple tokens with no space" $ do
	    let result = parse (some lexme) "fakeFile" "if(123)"
	    result `shouldBe` Right [TokKeyword "if", TokLParen, TokInt 123, TokRParen]
