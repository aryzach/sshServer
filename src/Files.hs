module Files (getFiles) where

import System.IO
import System.Directory

getFilesInDirectory :: FilePath -> IO [FilePath]
getFilesInDirectory dirPath = do
  entries <- getDirectoryContents dirPath
  let filenames = filter (`notElem` [".", ".."]) entries
  return filenames

getFiles = getFilesInDirectory "./files/"

{-readFile :: String -> IO String
readFile fn = do
  file <- openFile fn ReadMode
  contents <- hGetContents file
  hClose file
  pure contents
  -}
