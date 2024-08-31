{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec (parse)

import FileReader
import Lexer


main :: IO ()
main = do
    fileName <- getFileName
    contents <- FileReader.readFile fileName

    case parse lexme fileName contents of
        Left err -> putStrLn $ errorBundlePretty err
        Right tokens -> print tokens
