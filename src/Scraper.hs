{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import           Network.HTTP.Simple
import           Types
import           Data.Maybe
import           Data.Map
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.ByteString.Lazy as BS
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree
import           Text.Read (readMaybe)
import           Data.Char (isDigit)
import qualified Config
import           Data.Text.Encoding.Error (UnicodeException)


getCourse :: (CourseId, Year) -> IO (Either JSONException Course)
getCourse (courseId,year) = do
  request <- parseRequest $ Config.courseUrl courseId year
  response <- httpJSONEither request
  return (getResponseBody response :: Either JSONException Course)

data ScraperError = RequestError UnicodeException | LinkExtractionError | IdExtractionError deriving Show

getCourseNames :: IO (Either ScraperError (Map CourseId T.Text))
getCourseNames = do
  req <- parseRequest Config.courseListUrl
  res <- httpLBS req
  return $ do
    body <- case decodeUtf8' $ BS.toStrict $ getResponseBody res
            of Left err -> Left $ RequestError err
               Right b -> Right b
    let validLinks' = [x | x@(TagBranch "a" [("href", href)] _) <- universeTree $ parseTree body, isOrariLink href]
    validLinks <- if length validLinks' <= 1 --at least a plurality of links
                  then Left LinkExtractionError --if we are here probably something went wrong
                  else Right validLinks'
    let names' = fromList $ catMaybes $ extractIdDescription <$> validLinks
    if length names' <= 1 --MAYBE even a single Nothing causes a Left to be returned, or an error to be logged (TODO research logging monad)
      then Left IdExtractionError
      else Right names'

extractIdDescription :: TagTree T.Text -> Maybe (CourseId, T.Text)
extractIdDescription (TagBranch "a" [("href", href)] [TagLeaf (TagText description)]) =
  (,) <$> courseIdMaybe <*> pure description
  where courseIdMaybe = readMaybe . T.unpack . T.takeWhileEnd isDigit . T.dropWhileEnd ('/'==) $ href
extractIdDescription _ = Nothing

isOrariLink :: T.Text -> Bool
isOrariLink = T.isPrefixOf Config.courseLinkPrefix

