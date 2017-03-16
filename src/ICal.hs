{-# LANGUAGE OverloadedStrings #-}
module ICal where

import Data.Monoid
import Data.Text (Text, pack, replace)
import Data.Time (UTCTime)
import Data.Time.Format
import Data.Maybe (fromMaybe)

data Event = Event { start :: UTCTime
                   , end :: UTCTime
                   , summary :: Text
                   , description :: Maybe Text
                   , location :: Maybe Text }


eventsToICal :: [Event] -> Text
eventsToICal = wrapICal . eventsToICalUnwrapped

eventsToICalUnwrapped :: [Event] -> Text
eventsToICalUnwrapped = foldMap encodeEvent

wrapICal :: Text -> Text
wrapICal body = "BEGIN:VCALENDAR\n"
             <> body
             <> "END:VCALENDAR\n"

encodeTime :: UTCTime -> Text
encodeTime = pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

escape :: Text -> Text
escape = replace "\n" "\\n"
       . replace "," "\\,"
       . replace ";" "\\;"
       . replace "\\" "\\\\"

encodeEvent :: Event -> Text
encodeEvent e =
  "BEGIN:VEVENT\n"
  <> required "DTSTART"     (encodeTime $ start e) 
  <> required "DTEND"       (encodeTime $ end e)
  <> required "SUMMARY"     (summary e)
  <> optional "DESCRIPTION" (description e)
  <> optional "LOCATION"    (location e)
  <> "END:VEVENT\n"
    where optional k v = fromMaybe "" ((\v' -> k <> ":" <> escape v' <> "\n") <$> v)
          required k v = k <> ":" <> escape v <> "\n"

