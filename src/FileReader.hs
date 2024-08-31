module FileReader (
    getFileName,
    FileReader.readFile
) where

import System.IO

getFileName :: IO FilePath
getFileName = do
    putStrLn "Enter the file name: "
    fileName <- getLine
    return fileName

readFile :: FilePath -> IO String
readFile filePath = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    -- No need to close as hGetContents does it for us
    return contents
        
