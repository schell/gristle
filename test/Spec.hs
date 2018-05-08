{-# LANGUAGE LambdaCase #-}

import           System.Directory (doesDirectoryExist, listDirectory, getCurrentDirectory)
import           System.FilePath  ((</>))
import           Test.DocTest


listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path = do
  contents <- listDirectory path
  fmap concat <$> sequence $ flip map contents
                           $ \file -> do
    let filepath = path </> file
    doesDirectoryExist filepath >>= \case
      False -> return [filepath]
      True  -> listDirectoryRecursive filepath


main :: IO ()
main = do
  files <- concat <$> mapM listDirectoryRecursive ["src", "app"]
  putStrLn "\n"
  putStrLn "doctesting files:"
  mapM_ putStrLn $ do
    file <- files
    return $ "  " ++ file
  doctest files
