module Main (main) where

import FileReader

main :: IO ()
main = do
    fileName <- getFileName
    contents <- FileReader.readFile fileName
    print contents
