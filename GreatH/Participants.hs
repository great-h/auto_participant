{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module GreatH.Participants (
  Participant,
  getParticipants,
  participantsMock
  ) where

import qualified Data.ByteString.Lazy as BS (readFile)
import qualified Data.Text.Lazy as TL

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attribute, fromDocument)
import Text.XML.Scraping (innerHtml)
import Text.XML.Selector.TH

import Network.HTTP.Conduit
import Control.Arrow ((&&&))


type Participant = (TL.Text, [TL.Text])


getParticipants :: String -> IO [Participant]
getParticipants uri = do
  root <- fmap (fromDocument . parseLBS) $ simpleHttp uri
  return $ users root


users :: Cursor -> [Participant]
users = map (username &&& socials)
        . queryT [jq| .user-profile |]

username :: Cursor -> TL.Text
username = innerHtml . queryT [jq| .user-name |]

socials :: Cursor -> [TL.Text]
socials =  map (TL.fromStrict . Prelude.head . attribute "href")
           . queryT [jq| .external-profile-link |]

file :: IO Cursor
file = fmap (fromDocument . parseLBS) $ BS.readFile "./23.html"

participantsMock :: IO [Participant]
participantsMock = fmap users file
