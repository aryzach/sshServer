module Files (getFiles) where

import System.Directory

getFilesInDirectory :: FilePath -> IO [FilePath]
getFilesInDirectory dirPath = do
  entries <- getDirectoryContents dirPath
  let filenames = filter (`notElem` [".", ".."]) entries
  return filenames

getFiles = getFilesInDirectory "./files/"
