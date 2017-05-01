{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Data.Aeson
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Data.Time.Format (ParseTime, parseTimeM, defaultTimeLocale)
import           Text.Read (readMaybe)
import           Data.Map.Strict
import           Data.ByteString.Lazy (ByteString)
import           Data.Default.Class

type CourseId = Int
type Year = Int
type SubjectId = Int
type EventId = Int

--MAYBE add color
data Subject = Subject { subjectId :: SubjectId
                       , subjectDescription :: Text } deriving Show

instance FromJSON Subject where
  parseJSON (Object v) = Subject
                     <$> v .: "IdADfisica"
                     <*> v .: "DescrizioneAD"
  parseJSON _ = mempty

--MAYBE add details
data Event = Event { eventSubjectId :: EventId
                   , eventTitle :: Text
                   , eventStart :: UTCTime
                   , eventEnd :: UTCTime } deriving Show

instance FromJSON Event where
  parseJSON (Object v) = Event
                     <$> ((v .: "id"   ) >>= (maybe mempty pure . readMaybe))
                     <*>   v .: "title"
                     <*> ((v .: "start") >>= parseEpochTime)
                     <*> ((v .: "end"  ) >>= parseEpochTime)
  parseJSON _ = mempty

parseEpochTime :: (Monad m, ParseTime t) => String -> m t
parseEpochTime = parseTimeM True defaultTimeLocale "%s"

data Course = Course { subjects :: [Subject]
                     , events :: [Event] } deriving Show

instance FromJSON Course where
  parseJSON (Object v) = Course
                     <$> v .: "Attivita"
                     <*> v .: "Eventi"
  parseJSON _ = mempty

-----

data Servable = Servable { servableCourses :: Map (CourseId,Year) Text
                         , servableClasses :: Map SubjectId Text
                         , servableJs :: ByteString
                         , servableHtml :: ByteString } deriving Show

instance Default Servable where
  def = Servable { servableCourses = empty
                 , servableClasses = empty
                 , servableJs = mempty
                 , servableHtml = mempty }

