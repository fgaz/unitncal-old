{-# LANGUAGE OverloadedStrings #-}
module Config where

import Network.Wai.Handler.Warp (HostPreference)
import Types
import Data.Monoid
import Data.Text (Text)

refreshInterval :: Int
refreshInterval = 1000000 -- one second
                * 60 -- one minute
                * 60 -- one hour

port :: Int
port = 8080

host :: HostPreference
host = "127.0.0.1"

courseListUrl :: String
courseListUrl = "https://webapps.unitn.it/Orari/it/Web/Dipartimento"

courseUrl :: CourseId -> Year -> String
courseUrl courseId year = "https://webapps.unitn.it/Orari/it/Web/AjaxEventi/c/"
                       <> show courseId
                       <> "-"
                       <> show year
                       <> "/agendaWeek?start=0&end=10000000000"

courseLinkPrefix :: Text
courseLinkPrefix = "/Orari/it/Web/CalendarioCds/"

