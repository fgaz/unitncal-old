{-# LANGUAGE OverloadedStrings #-}
module Types.Servable where

import Types
import Data.Default.Class
import Generator.Html (mkHtml)

import           Data.Text (Text)
import           Data.Map.Strict (Map, empty)
import           Data.ByteString.Lazy (ByteString)

data Servable = Servable { servableCourses :: Map (CourseId,Year) Text
                         , servableClasses :: Map SubjectId Text
                         , servableJs :: ByteString
                         , servableHtml :: ByteString } deriving Show

instance Default Servable where
  def = Servable { servableCourses = empty
                 , servableClasses = empty
                 , servableJs = "{}"
                 , servableHtml = mkHtml empty }

