module LexerSpec (
    spec
)where

-- Avoid conflicting import
import Text.Megaparsec hiding (Token)

import Test.Hspec
import Data.Either (isLeft)

import Lexer
import Token

spec :: Spec
spec = do
    let fileName = "fakeFile"

    describe "Lexer Base Data Types" $ do
        it "should lex integers" $ do
	    let result = parse lexme fileName "123"
            result `shouldBe` Right (TokInt 123)

	it "should lex negative integers" $ do
	    let result = parse lexme fileName "-123"
	    result `shouldBe` Right (TokInt (-123))

	it "should lex char" $ do
	    let result = parse lexme fileName "\"a\""
	    result `shouldBe` Right (TokString "a")

	it "should lex strings" $ do
	    let result = parse lexme fileName "\"hello\""
	    result `shouldBe` Right (TokString "hello")

	it "should lex null" $ do
	    let result = parse lexme fileName "null"
	    result `shouldBe` Right TokNull

	it "should lex True" $ do
	    let result = parse lexme fileName "True"
	    result `shouldBe` Right (TokBool True)

	it "should lex False" $ do
	    let result = parse lexme fileName "False"
	    result `shouldBe` Right (TokBool False)

	it "should lex comma" $ do
	    let result = parse lexme fileName ","
	    result `shouldBe` Right TokComma

	it "should lex colon" $ do
	    let result = parse lexme fileName ":"
	    result `shouldBe` Right TokColon

	it "should lex dot" $ do
	    let result = parse lexme fileName "."
	    result `shouldBe` Right TokDot

	it "should lex float" $ do
	    let result = parse lexme fileName "3.14"
	    result `shouldBe` Right (TokFloat 3.14)

	it "should lex float with 0 decimal" $ do
	    let result = parse lexme fileName "3.0"
	    result `shouldBe` Right (TokFloat 3.0)

	it "should lex float with 0 integer" $ do
	    let result = parse lexme fileName "0.14"
	    result `shouldBe` Right (TokFloat 0.14)

	it "should lex float with negative" $ do
	    let result = parse lexme fileName "-3.14"
	    result `shouldBe` Right (TokFloat (-3.14))

    describe "Lexer brackets" $ do
	it "should lex left parentheses" $ do
	    let result = parse lexme fileName "("
	    result `shouldBe` Right TokLParen

	it "should lex right parentheses" $ do
	    let result = parse lexme fileName ")"
	    result `shouldBe` Right TokRParen

	it "should lex left bracket" $ do
	    let result = parse lexme fileName "["
	    result `shouldBe` Right TokLBrack

	it "should lex right bracket" $ do
	    let result = parse lexme fileName "]"
	    result `shouldBe` Right TokRBrack

	it "should lex left braces" $ do
	    let result = parse lexme fileName "{"
	    result `shouldBe` Right TokLBrace

	it "should lex right braces" $ do
	    let result = parse lexme fileName "}"
	    result `shouldBe` Right TokRBrace

    describe "Lexer Identifiers" $ do
	it "should lex single letter identifiers" $ do
	    let result = parse lexme fileName "x"
	    result `shouldBe` Right (TokIdent "x")

	it "should lex multiple letter identifiers" $ do
	    let result = parse lexme fileName "hello"
	    result `shouldBe` Right (TokIdent "hello")

	it "should lex identifiers with numbers" $ do
	    let result = parse lexme fileName "x123"
	    result `shouldBe` Right (TokIdent "x123")

	it "should lex identifiers with underscores" $ do
	    let result = parse lexme fileName "x_y_z"
	    result `shouldBe` Right (TokIdent "x_y_z")

    describe "Lexer Keywords" $ do
	it "should lex if" $ do
	    let result = parse lexme fileName "if"
	    result `shouldBe` Right (TokKeyword "if")

	it "should lex else" $ do
	    let result = parse lexme fileName "else"
	    result `shouldBe` Right (TokKeyword "else")

	it "should lex while" $ do
	    let result = parse lexme fileName "while"
	    result `shouldBe` Right (TokKeyword "while")

	it "should lex func" $ do
	    let result = parse lexme fileName "func"
	    result `shouldBe` Right (TokKeyword "func")

    describe "Lexer Operators" $ do
	it "should lex +" $ do
	    let result = parse lexme fileName "+"
	    result `shouldBe` Right (TokOp "+")

	it "should lex -" $ do
	    let result = parse lexme fileName "-"
	    result `shouldBe` Right (TokOp "-")

	it "should lex *" $ do
	    let result = parse lexme fileName "*"
	    result `shouldBe` Right (TokOp "*")
	
	it "should lex /" $ do
	    let result = parse lexme fileName "/"
	    result `shouldBe` Right (TokOp "/")

	it "should lex &&" $ do
	    let result = parse lexme fileName "&&"
	    result `shouldBe` Right (TokOp "&&")
	    
	it "should lex ||" $ do
	    let result = parse lexme fileName "||"
	    result `shouldBe` Right (TokOp "||")

	it "should lex ==" $ do
	    let result = parse lexme fileName "=="
	    result `shouldBe` Right (TokOp "==")
	    
	it "should lex !=" $ do
	    let result = parse lexme fileName "!="
	    result `shouldBe` Right (TokOp "!=")
	    
	it "should lex <" $ do
	    let result = parse lexme fileName "<"
	    result `shouldBe` Right (TokOp "<")

	it "should lex >" $ do
	    let result = parse lexme fileName ">"
	    result `shouldBe` Right (TokOp ">")

	it "should lex <=" $ do
	    let result = parse lexme fileName "<="
	    result `shouldBe` Right (TokOp "<=")
	    
	it "should lex >=" $ do
	    let result = parse lexme fileName ">="
	    result `shouldBe` Right (TokOp ">=")

	it "should lex =" $ do
	    let result = parse lexme fileName "="
	    result `shouldBe` Right (TokOp "=")
	
    describe "Lexer comments" $ do
	it "should lex single line comments" $ do
	    let result = parse lexme fileName "// This is a comment\n123"
	    result `shouldBe` Right (TokInt 123)

	it "should lex block comments" $ do
	    let result = parse lexme fileName "/* This is a comment */123"
	    result `shouldBe` Right (TokInt 123)

    describe "Lexer Multiple Tokens" $ do
	it "should lex multiple ints" $ do
	    let result = parse (some lexme) fileName "123 456 789"
	    result `shouldBe` Right [TokInt 123, TokInt 456, TokInt 789]

	it "should lex multiple ops" $ do
	    let result = parse (some lexme) fileName "+-*/"
	    result `shouldBe` Right [TokOp "+", TokOp "-", TokOp "*", TokOp "/"]

	it "should lex multiple tokens" $ do
	    let result = parse (some lexme) fileName "123 \"hello\" if"
	    result `shouldBe` Right [TokInt 123, TokString "hello", TokKeyword "if"]

	it "should lex multiple tokens with no space" $ do
	    let result = parse (some lexme) fileName "if(123)"
	    result `shouldBe` Right [TokKeyword "if", TokLParen, TokInt 123, TokRParen]

	it "should lex multiple tokens with space" $ do
	    let result = parse (some lexme) fileName "if ( 123 )"
	    result `shouldBe` Right [TokKeyword "if", TokLParen, TokInt 123, TokRParen]

	it "should lex arrays" $ do
	    let result = parse (some lexme) fileName "[1, 2, 3]"
	    result `shouldBe` Right [TokLBrack, TokInt 1, TokComma, TokInt 2, TokComma, TokInt 3, TokRBrack]

	it "should lex function calls" $ do
	    let result = parse (some lexme) fileName "func(1, 2, 3)"
	    result `shouldBe` Right [TokKeyword "func", TokLParen, TokInt 1, TokComma, TokInt 2, TokComma, TokInt 3, TokRParen]

    describe "Lexer Errors" $ do
	it "should fail on invalid int" $ do
	    let result = parse lexme fileName "123a"
	    result `shouldSatisfy` isLeft

	it "should fail on invalid float" $ do
	    let result = parse lexme fileName "3.14.15"
	    result `shouldSatisfy` isLeft

	it "should fail on invalid string" $ do
	    let result = parse lexme fileName "\"hello"
	    result `shouldSatisfy` isLeft

	it "should fail on unclosed string" $ do
	    let result = parse lexme fileName "\"hello"
	    result `shouldSatisfy` isLeft

	it "should fail on unclosed block comments" $ do
	    let result = parse lexme fileName "/* This is a comment"
	    result `shouldSatisfy` isLeft

    describe "Lexer typed parameters" $ do
	it "should lex typed parameter" $ do
	    let result = parse (some lexme) fileName "(x: int)"
	    result `shouldBe` Right [TokLParen, TokIdent "x", TokColon, TokIdent "int", TokRParen]

	it "should lex typed parameters" $ do
	    let result = parse (some lexme) fileName "(x: int, y: float)"
	    result `shouldBe` Right [TokLParen, TokIdent "x", TokColon, TokIdent "int", TokComma, TokIdent "y", TokColon, TokIdent "float", TokRParen]

	it "should lex typed parameters with spaces" $ do
	    let result = parse (some lexme) fileName "( x : int , y : float )"
	    result `shouldBe` Right [TokLParen, TokIdent "x", TokColon, TokIdent "int", TokComma, TokIdent "y", TokColon, TokIdent "float", TokRParen]
