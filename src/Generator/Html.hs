{-# LANGUAGE OverloadedStrings #-}

module Generator.Html where

import Lucid
import Data.Monoid
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack)
import Data.Map.Strict
import Types

-- TODO is it worth the additional complexity to pre-render the html? is the performance gain significant?
-- MAYBE just use servant-lucid
mkHtml :: Map CourseId Text -> ByteString
mkHtml courseNames = renderBS $ wrapHtml $ do
        section_ $ p_ "Questo sito raccoglie automaticamente ogni ora gli orari delle lezioni dal sito di unitn e li converte in un formato standard (icalendar) utilizzabile dalla maggior parte di app e programmi calendario."
        section_ $ header_ $ h2_ "Lista dei calendari"
        mkCourseList courseNames

-- MAYBE use this to render the other pages too
wrapHtml :: Html () -> Html ()
wrapHtml inner = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ "Unitn iCal files"
      metas
      assets
    body_ $ do
      header
      div_ [class_ "wrap"] $ main_ [role_ "main"] inner
      hr_ []
      footer

mkCourseList :: Map CourseId Text -> Html ()
mkCourseList courseNames = ul_ [class_ "col_ul"] $ foldMap (uncurry toEntry) $ toList courseNames

toEntry :: CourseId -> Text -> Html ()
toEntry courseId description = li_ $ do
  span_ $ toHtml description
  ol_ $ mapM_ (uncurry mkUrl) $ zip [1..] ["Primo anno", "Secondo anno", "Terzo anno", "Quarto anno", "Quinto anno"]
  where mkUrl :: Year -> Text -> Html ()
        mkUrl year name = li_ $ a_ [href_ ("course/" <> pack (show courseId) <> "/" <> pack (show year))] $ toHtml name

metas :: Html ()
metas = do
  meta_ [name_ "description", content_ "Unitn iCal files, updated every hour"]
  meta_ [name_ "keywords"   , content_ "unitn,ical,icalendar,università,trento,calendario,orari,corsi"]
  meta_ [name_ "author"     , content_ "Francesco Gazzetta"]
  meta_ [name_ "viewport"   , content_ "width=device-width, initial-scale=1.0, shrink-to-fit=no"]

assets :: Html ()
assets = do
  link_ [href_ "/css/normalize.css", rel_ "stylesheet", media_ "all"]
  link_ [href_ "/css/main.css", rel_ "stylesheet", media_ "all"]
  script_ [src_ "/js/expandlist.js", type_ "text/javascript", media_ "all"] (mempty :: Html ())

header :: Html ()
header = header_ [role_ "banner"] $ do
  h1_ "Unitn iCal files"
  nav_ [role_ "navigation"] $
    ul_ $ do
      li_ $ a_ [href_ "/"] "Home"
      li_ $ a_ [href_ "/about.html"] "About"
      li_ $ a_ [href_ "/instructions.html"] "Istruzioni"
      li_ $ a_ [href_ "/multical.html"] "Calendario personalizzato"

footer :: Html ()
footer = footer_ [role_ "contentinfo"] $ do
  p_ $ do
    "Sviluppato da "
    a_ [href_ "http://fgaz.me"] "Francesco Gazzetta"
  p_ $ do
    "Sorgenti: "
    a_ [href_ "https://github.com/fgaz/unitncal"] "https://github.com/fgaz/unitncal"

