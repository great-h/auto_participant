module GreatH.RecentPost (recentPost) where

import Control.Monad (mzero)
import System.Directory (getDirectoryContents)
import Text.Parsec

-- _posts ディレクトリの最新のファイルを取得する
recentPost :: IO FilePath
recentPost = do
  let directory = "_posts/"
  files <- getDirectoryContents directory
  return . snd . maximum $ do
    file <- files
    case parse filename "" file of
      Left err -> mzero
      Right xs -> return (xs, directory ++ file)

  where filename = do
                  year <- many1 digit
                  oneOf "-"
                  month <- many1 digit
                  oneOf "-"
                  day <- many1 digit
                  anyChar
                  return $ read year * 10000 + read month * 100 + read day
