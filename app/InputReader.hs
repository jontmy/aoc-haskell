module InputReader where

readStrings :: FilePath -> IO [String]
readStrings path = do
  input <- readFile path
  return (lines input)
  
readIntegers :: FilePath -> IO [Integer]
readIntegers path = do
  input <- readFile path
  return . map read . lines $ input