module Generator where

import Types
import Scraper
import ICal
import Data.Text (Text)
import Data.Map.Strict
import Data.Either (rights)
import Prelude as P hiding (lookup)
import Data.Maybe (fromMaybe)
import qualified Generator.Js as Js
import qualified Generator.Html as Html
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Simple (JSONException)


toStandardEvent :: Map SubjectId Text -> Types.Event -> ICal.Event
toStandardEvent subjs e = ICal.Event { start = eventStart e
                                     , end = eventEnd e
                                     , summary = fromMaybe (eventTitle e {-fallback-}) $ lookup (eventSubjectId e) subjs
                                     , description = Just $ eventTitle e
                                     , location = extractLocation e }

extractLocation :: Types.Event -> Maybe Text
extractLocation = const Nothing --TODO

mkCourseCal :: Course -> Text
mkCourseCal c = eventsToICal $ toStandardEvent subjs <$> es
  where es = events c
        subjs = fromList $ (\(Subject k v) -> (k,v)) <$> subjects c --TODO this probably isn't the best way/place to do it

mkClassCals :: Course -> Map SubjectId Text
mkClassCals c = mapWithKey (\k _ -> eventsToICalUnwrapped $ toStandardEvent subjs <$> P.filter (\e -> eventSubjectId e == k) es) subjs
  where es = events c
        subjs = fromList $ (\(Subject k v) -> (k,v)) <$> subjects c --TODO this probably isn't the best way/place to do it

mkServable :: [(CourseId, Year)] -> [Course] -> ByteString -> ByteString -> Servable
mkServable courseKeys courseData js html = servable
  where
    coursesMap = fromList $ P.zip courseKeys courseData --MAYBE zip is ugly, return the keys too
    courses = fmap mkCourseCal coursesMap
    classes = foldMap mkClassCals coursesMap
    servable = Servable courses classes js html

data GeneratorError = ScraperError ScraperError | CourseParsingError JSONException deriving Show

makeNewServable :: IO (Either GeneratorError Servable)
makeNewServable = do
  courseNames' <- getCourseNames
  --TODO just use a monad/transformer and refactor this mess
  case courseNames' of Left err -> return $ Left $ ScraperError err
                       Right courseNames -> makeNewServable' courseNames
  where
    makeNewServable' :: Map CourseId Text -> IO (Either GeneratorError Servable)
    makeNewServable' courseNames = do
      let courseKeys = (,) <$> keys courseNames <*> [1..5]
      courseData' <- traverse getCourse courseKeys --TODO merge dbs and log errors
      return $ case sequenceA courseData' of Left err -> Left $ CourseParsingError err
                                             Right courseData -> Right servable
                                               where
                                                 js = Js.jsonToJs $ Js.mkJs courseNames courseKeys courseData
                                                 html = Html.mkHtml courseNames
                                                 servable = mkServable courseKeys courseData js html --MAYBE avoid to pass js and html?

