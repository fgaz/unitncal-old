{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import           Network.HTTP.Simple
import           Types
import           Data.Monoid
import           Data.Maybe
import           Data.Map
import           Data.Text as T
import           Data.Text.Encoding
import           Data.ByteString.Lazy as BS
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree
import           Text.Read (readMaybe)
import           Data.Char (isDigit)



courseUrl :: CourseId -> Year -> String
courseUrl courseId year = "https://webapps.unitn.it/Orari/it/Web/AjaxEventi/c/"
                       <> show courseId
                       <> "-"
                       <> show year
                       <> "/agendaWeek?start=0&end=10000000000"

getCourse :: (CourseId, Year) -> IO (Either JSONException Course)
getCourse (courseId,year) = do
  request <- parseRequest $ courseUrl courseId year
  response <- httpJSONEither request
  return (getResponseBody response :: Either JSONException Course)

getCourseNames :: IO (Map CourseId Text)
getCourseNames = do
  req <- parseRequest "https://webapps.unitn.it/Orari/it/Web/Dipartimento"
  res <- httpLBS req
  let Right body = decodeUtf8' $ BS.toStrict $ getResponseBody res --TODO handle error
  let validLinks = [x | x@(TagBranch "a" [("href", href)] _) <- universeTree $ parseTree body, isOrariLink href]
  return $ fromList $ catMaybes $ extractIdDescription <$> validLinks --MAYBE log errors?

extractIdDescription :: TagTree Text -> Maybe (CourseId, Text)
extractIdDescription (TagBranch "a" [("href", href)] [TagLeaf (TagText description)]) =
  (,) <$> courseIdMaybe <*> pure description
  where courseIdMaybe = readMaybe . T.unpack . takeWhileEnd isDigit . dropWhileEnd ('/'==) $ href
extractIdDescription _ = Nothing

isOrariLink :: Text -> Bool
isOrariLink = T.isPrefixOf "/Orari/it/Web/CalendarioCds/"

