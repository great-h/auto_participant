{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text as TI
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.Maybe
import Control.Monad

import Data.Text.Format

import System.IO (Handle, IOMode(..), withFile)

import GreatH.RecentPost
import GreatH.Participants

import Network.URI
import Text.Parsec
import Data.Yaml
import qualified Data.HashMap.Strict as M

main = do
  filename <- recentPost
  uri <- getDoorkeeperURL filename
  participants <- getParticipants uri
  defaultUsers <- getDefaultUsers
  let markdown = createParitipantsMarkdown
                 . map (mergeDefault defaultUsers)
                 $ participants
  forM_ markdown $ withFile filename AppendMode . flip T.hPutStr

getDefaultUsers = do
  users <- decodeFile "users.yml"
  return . fromJust $ users

getDoorkeeperURL :: FilePath -> IO String
getDoorkeeperURL path = do
  maybe_object <- getFrontFormat path
  maybe_string <- return $ fromJust $ do
    object <- maybe_object
    M.lookup "doorkeeper" $ object
  case maybe_string of
    String value -> return . TI.unpack $ value


mergeDefault registUsers (participantName, urls) = (name, url)

  where participantUrl = getParticipantUrl urls
        identity = getID participantUrl participantName
        maybeUserValue = M.lookup identity registUsers
        name = getName identity maybeUserValue
        url = getUrl participantUrl maybeUserValue

getParticipantUrl = head . getParticipantUrls
getParticipantUrls urls = [ url |
                is <- [isGitHub, isTwitter, isFacebook, isLinkedIn],
                url <- urls,
                is url ]

getID url name =
  case parse parser "" $ T.unpack url of
    Left err -> name
    Right str -> T.pack str
  where parser = (many $ noneOf "/" ) `sepBy` oneOf "/" >>= return . last

getName id maybeUserValue = case maybeName of
    Just name -> name
    Nothing -> id

  where maybeName = do
          userValue <- maybeUserValue
          M.lookup ("name" :: B.ByteString) userValue

getUrl url maybeUserValue = case maybeUrl of
    Just u -> u
    Nothing -> url

  where maybeUrl = do
          userValue <- maybeUserValue
          M.lookup "url" userValue

createParitipantsMarkdown :: [(T.Text, T.Text)] -> [T.Text]
createParitipantsMarkdown xs =
  map (\ (n, uri) -> format "\n\n## [{}]({})\n" (n, uri)) xs

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
  auth <- (parseURI $ T.unpack str) >>= uriAuthority
  guard $ (T.pack $ uriRegName auth)  == valid


getFrontFormat :: FilePath -> IO (Maybe Object)
getFrontFormat path = do
  file <- B.readFile path
  return . decode . parseFrontFormat $ file

parseFrontFormat :: B.ByteString -> B.ByteString
parseFrontFormat text = case parse parser "" text of
  Left err -> ""
  Right str -> str

  where parser = do
          string "---\n"
          notFollowedBy $ oneOf "\n"
          rawYamlLines <- manyTill anyChar $ try (string "---\n")
          optional $ oneOf "\n"
          return . BC.pack $ rawYamlLines
