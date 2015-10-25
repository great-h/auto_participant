{-# OPTIONS -Wall -Werror #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import Prelude
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text as TI
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.Maybe
import Data.String (IsString)
import Data.Hashable (Hashable)
import Control.Monad

import Data.Text.Format

import System.IO (IOMode(..), withFile)

import GreatH.RecentPost
import GreatH.Participants

import Network.URI
import Text.Parsec
import Data.Yaml
import qualified Data.HashMap.Strict as M

instance FromJSON (M.HashMap BC.ByteString T.Text) where
  parseJSON value = parseJSON value >>= return . translate
    where
      translate = M.fromList . map pack . M.toList
      pack (k, v) = (BC.pack k, v)

main :: IO ()
main = do
  filename <- recentPost
  uri <- getDoorkeeperURL filename
  participants <- getParticipants uri
  defaultUsers <- getDefaultUsers
  let markdown = createParitipantsMarkdown
                 . map (mergeDefault defaultUsers)
                 $ participants
  forM_ markdown $ withFile filename AppendMode . flip T.hPutStr

getDefaultUsers :: IO (M.HashMap T.Text (M.HashMap BC.ByteString T.Text))
getDefaultUsers = do
  users <- decodeFile "users.yml"
  return . fromJust $ users

getDoorkeeperURL :: FilePath -> IO String
getDoorkeeperURL filepath = do
  maybe_object <- getFrontFormat filepath
  let str = fromJust $ do
        obj <- maybe_object
        M.lookup "doorkeeper" obj
  case str of
    String value -> return $ (TI.unpack $ value) ++ "/participants"
    _ -> error "not parameter doorkeeper"


mergeDefault :: M.HashMap T.Text (M.HashMap BC.ByteString T.Text)
                      -> (T.Text, [T.Text]) -> (T.Text, Maybe T.Text)
mergeDefault registUsers (participantName, urls) = (name, url)

  where participantUrl = getParticipantUrl urls
        identity = getID participantUrl participantName
        maybeUserValue = M.lookup identity registUsers
        name = getName identity maybeUserValue
        url = getUrl participantUrl maybeUserValue

getParticipantUrl :: [T.Text] -> Maybe T.Text
getParticipantUrl urls = case urls of
  ( [ ] ) -> Nothing
  _ -> return . head . getParticipantUrls $ urls
getParticipantUrls :: [T.Text] -> [T.Text]
getParticipantUrls urls = [ url |
                is <- [isGitHub, isTwitter, isFacebook, isLinkedIn],
                url <- urls,
                is url ]

getID :: Maybe T.Text -> T.Text -> T.Text
getID murl name =
  case murl of
    ( Just url ) -> case parse parser "" $ T.unpack url of
      Left _ -> name
      Right str -> T.pack str
    Nothing -> name
    where parser = liftM last $ many (noneOf "/" ) `sepBy` oneOf "/"

getName :: forall b. b -> Maybe (M.HashMap BC.ByteString b) -> b
getName identity maybeUserValue = fromMaybe identity maybeName

  where maybeName = do
          userValue <- maybeUserValue
          M.lookup ("name" :: B.ByteString) userValue

getUrl :: forall b k.
                (Eq k, Data.String.IsString k, Data.Hashable.Hashable k) =>
                Maybe b -> Maybe (M.HashMap k b) -> Maybe b
getUrl murl maybeUserValue = fromMaybe murl maybeUrl

  where maybeUrl = do
          userValue <- maybeUserValue
          return $ M.lookup "url" userValue

createParitipantsMarkdown :: [(T.Text, Maybe T.Text)] -> [T.Text]
createParitipantsMarkdown =
  map $ \ (n, muri) -> case muri of
    ( Just uri ) -> format "\n\n## [{}]({})\n" (n, uri)
    Nothing -> format "\n\n## {}\n" $ Only n

isFacebook :: T.Text -> Bool
isFacebook = isServiceBase "www.facebook.com"

isGitHub :: T.Text -> Bool
isGitHub = isServiceBase "github.com"

isTwitter :: T.Text -> Bool
isTwitter = isServiceBase "twitter.com"

isLinkedIn :: T.Text -> Bool
isLinkedIn = isServiceBase "www.linkedin.com"

isServiceBase :: T.Text -> T.Text -> Bool
isServiceBase valid str = isJust $ do
  auth <- parseURI (T.unpack str) >>= uriAuthority
  guard (T.pack (uriRegName auth)  == valid)


getFrontFormat :: FilePath -> IO (Maybe Object)
getFrontFormat filepath = do
  file <- B.readFile filepath
  return . decode . parseFrontFormat $ file

parseFrontFormat :: B.ByteString -> B.ByteString
parseFrontFormat text = case parse parser "" text of
  Left _ -> ""
  Right str -> str

  where parser = do
          _ <- string "---\n"
          _ <- notFollowedBy $ oneOf "\n"
          rawYamlLines <- manyTill anyChar $ try (string "---\n")
          _ <- optional $ oneOf "\n"
          return . BC.pack $ rawYamlLines
