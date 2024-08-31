import System.IO
import Control.Monad

readFile :: FilePath -> IO String
readFile filePath = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    print contents 
    hClose handle
        
