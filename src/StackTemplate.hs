module StackTemplate where

import           Control.Monad
import           System.Directory

writeTemplate :: IO ()
writeTemplate = template >>= writeFile "template.hsfiles"

template :: IO String
template = fmap bar readContents

bar :: [(FilePath, String)] -> String
bar = foldl appendTemplate ""

appendTemplate :: String -> (FilePath, String) -> String
appendTemplate template (path, content) = unlines [template, bar2 path, content]

bar2 :: FilePath -> String
bar2 path = "{-# START_FILE " ++ path ++ " #-}"

readContents :: IO [(FilePath, String)]
readContents = getFiles >>= mapM readContent

getFiles :: IO [FilePath]
getFiles = getCurrentDirectory >>= getDirectoryContents >>= filterM doesFileExist

readContent :: FilePath -> IO (FilePath, String)
readContent path = do
  content <- readFile path
  return (path, content)
