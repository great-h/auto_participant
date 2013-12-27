{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module GreatH.Participants (Participant, getParticipants, participants_mock) where

import Text.XML.Cursor (fromDocument)
import Text.HTML.DOM (parseLBS)
import qualified Data.ByteString.Lazy as BS (readFile,unpack)
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL --  (concat,unpack,fromStrict)

import Text.XML.Cursor
import Text.XML.Scraping (innerHtml, getMeta, eclass)
import Text.XML.Selector.TH

import Network.HTTP.Conduit
import Control.Arrow ((&&&))

type Participant = (TL.Text, [TL.Text])


getParticipants :: String -> IO [Participant]
getParticipants uri = do
  root <- fmap (fromDocument . parseLBS) $ simpleHttp uri
  return $ users root


users :: Cursor -> [Participant]
users = map ((username &&& socials))
          . queryT [jq| .user-profile |]

username = innerHtml . queryT [jq| .user-name |]
socials =  map (TL.fromStrict . Prelude.head . attribute "href")
           . queryT [jq| .external-profile-link |]
file = fmap (fromDocument . parseLBS) $ BS.readFile "./23.html"

participants_mock = file >>= return . users
