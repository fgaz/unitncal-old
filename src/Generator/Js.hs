{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Generator.Js where

import Types
import Data.Map
import Data.Text (Text, pack)
import Data.Aeson
import GHC.Generics (Generic)
import Data.Monoid
import Data.ByteString.Lazy (ByteString)

type CourseIdYear = Text

--TODO find a place for this.
data ClientJsIR = ClientJsIR { courses :: Map CourseId Text
                             , subjects :: Map CourseIdYear [(SubjectId, Text)] } deriving (Generic)

instance ToJSON ClientJsIR

mkJs :: Map CourseId Text -> [(CourseId, Year)] -> [Course] -> Value
mkJs courseNames courseKeys courseData = js
  where
    coursesList = fromList $ zip (mkJsKey <$> courseKeys) (mkJsValue <$> courseData) --MAYBE zip is ugly, return the keys too. also, duplicated code.
    js = toJSON $ ClientJsIR courseNames coursesList
    mkJsKey (a,b) = pack (show a) <> "-" <> pack (show b)
    mkJsValue = fmap (\(Subject a b) -> (a,b)) . Types.subjects

jsonToJs :: Value -> ByteString
jsonToJs j = "var data = " <> encode j <> ";\n"

