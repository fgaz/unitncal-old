{-# LANGUAGE OverloadedStrings #-}

module Generator.Html where

import Lucid
import Data.Monoid
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack)
import Data.Map.Strict
import Types
import Config (basePath)

-- TODO is it worth the additional complexity to pre-render the html? is the performance gain significant?
-- MAYBE just use servant-lucid
mkHtml :: Map CourseId Text -> ByteString
mkHtml courseNames = renderBS $ wrapHtml Nothing "" "" $ do
        section_ $ p_ "Questo sito raccoglie automaticamente ogni ora gli orari delle lezioni dal sito di unitn e li converte in un formato standard (icalendar) utilizzabile dalla maggior parte di app e programmi calendario."
        section_ $ header_ $ h2_ "Lista dei calendari"
        mkCourseList courseNames

-- MAYBE use this to render the other pages too
wrapHtml :: Maybe Text -> Text -> Html () -> Html () -> Html ()
wrapHtml title onload extraMetas inner = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ $ toHtml ("Unitn iCal files" <> maybe "" (" - " <>) title)
      metas
      extraMetas
      assets
    body_ [onload_ onload] $ do
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
        mkUrl year name = li_ $ a_ [href_ (basePath <> "/course/" <> pack (show courseId) <> "/" <> pack (show year))] $ toHtml name

metas :: Html ()
metas = do
  meta_ [name_ "description", content_ "Unitn iCal files, updated every hour"]
  meta_ [name_ "keywords"   , content_ "unitn,ical,icalendar,università,trento,calendario,orari,corsi"]
  meta_ [name_ "author"     , content_ "Francesco Gazzetta"]
  meta_ [name_ "viewport"   , content_ "width=device-width, initial-scale=1.0, shrink-to-fit=no"]

assets :: Html ()
assets = do
  link_ [href_ (basePath <> "/css/normalize.css"), rel_ "stylesheet", media_ "all"]
  link_ [href_ (basePath <> "/css/main.css"), rel_ "stylesheet", media_ "all"]
  script_ [src_ (basePath <> "/js/expandlist.js"), type_ "text/javascript", media_ "all"] (mempty :: Html ())

header :: Html ()
header = header_ [role_ "banner"] $ do
  h1_ "Unitn iCal files"
  nav_ [role_ "navigation"] $
    ul_ $ do
      li_ $ a_ [href_ (basePath <> "/")] "Home"
      li_ $ a_ [href_ (basePath <> "/about.html")] "About"
      li_ $ a_ [href_ (basePath <> "/instructions.html")] "Istruzioni"
      li_ $ a_ [href_ (basePath <> "/multical.html")] "Calendario personalizzato"

footer :: Html ()
footer = footer_ [role_ "contentinfo"] $ do
  p_ $ do
    "Sviluppato da "
    a_ [href_ "http://fgaz.me"] "Francesco Gazzetta"
  p_ $ do
    "Sorgenti: "
    a_ [href_ "https://github.com/fgaz/unitncal"] "https://github.com/fgaz/unitncal"

multicalMetas :: Html ()
multicalMetas = do
  link_ [href_ (basePath <> "/css/multical.css"), rel_ "stylesheet", media_ "all"]
  script_ [src_ (basePath <> "/js/data.js"), type_ "text/javascript", media_ "all"] (mempty :: Html ())
  script_ [src_ (basePath <> "/js/multical.js"), type_ "text/javascript", media_ "all"] (mempty :: Html ())

---
--pages
---

aboutPage :: Html ()
aboutPage = wrapHtml (Just "About") "" "" $
  section_ $ do
    header_ $ h2_ "About"
    p_ $ do
      "Il sito dell'Università di Trento mette a disposizione i calendari delle lezioni tramite una "
      a_ [href_ "http://webapps.unitn.it/Orari/it/Web/Dipartimento"] "webapp"
      " che non offre nessuna possibilità di sincronizzazione o esportazione verso un formato standard."
    p_ $ do
      "Unitncal è nato per ovviare a questo problema, convertendo automaticamente "
      "ogni ora i calendari in formato standard iCal (.ics) e mettendoli a "
      "disposizione di tutti gli studenti su indirizzi pubblici, in modo da "
      "renderne estremamente facile la "
      a_ [href_ "/instructions.html"] "sincronizzazione"
      "."
    p_ $ do
      "Il programma è disponibile sotto licenza libera: "
      a_ [href_ "https://github.com/fgaz/unitncal"] "https://github.com/fgaz/unitncal"
      ". "
      "Se riscontri problemi con il sito o trovi degli errori puoi creare una pull "
      "request o un'issue su github, o contattarmi direttamente all'indirizzo "
      "francygazz (chiocciola) gmail (punto) com."
      
multicalPage :: Html ()
multicalPage = wrapHtml (Just "Calendario personalizzato") "load();" multicalMetas $ do
  h2_ [style_ "text-align:center"] "Calendario personalizzato"
  div_ [id_ "selects"] $ do
    select_ [id_ "course", onchange_ "updatecourse()"] $
      option_ [id_ "defaultoptioncourse", disabled_ "true", selected_ "true"] "Scegli l'indirizzo"
    br_ []
    select_ [id_ "year", onchange_ "updateyear()"] $ do
      option_ [id_ "defaultoptionyear", disabled_ "true", selected_ "true"] "Scegli l'anno"
      mapM_ (\n -> option_ [value_ n] $ toHtml n) ["1", "2", "3", "4", "5"]
    br_ []
    select_ [id_ "subject", onchange_ "updatesubject()"] $
      option_ [id_ "defaultoptionsubject", disabled_ "true", selected_ "true"] "Scegli il corso"
  div_ [id_ "buttons"] $ do
    button_ [id_ "submit", onclick_ "submit()"] "Aggiungi"
    button_ [id_ "cancel", onclick_ "cancel()", disabled_ "true"] "Annulla"
  input_ [type_ "text", id_ "link", readonly_ "true"]

instructionsPage :: Html ()
instructionsPage =
  wrapHtml (Just "Instructions") "" "" $ section_ [] $ do
    header_ [] $ h2_ [] "Istruzioni"

    p_ [] "cortesia di Nicola Sartorato"

    p_ [] $ a_ [name_ "copialink"] $ strong_ [] "Per copiare il link del calendario:"

    ol_ [] $ do
      li_ [] $ do
        "Collegarsi alla "
        a_ [href_ "http://unitncal.fgaz.me/"] "Home"
        ";"
      li_ [] "Cliccare sopra il proprio dipartimento e corso di laurea;"
      li_ [] $ do
        "Cliccare con il tasto destro (pressione a lungo se da smartphone) sopra l'anno desiderato e selezionare "
        strong_ [] "Copia indirizzo link"
        "."
    p_ [] $ do
      a_ [name_ "copialink"] ""
      strong_ [] "Scegli la procedura da seguire a seconda della tipologia di smartphone:"

    a_ [name_ "android", href_ "android.html"] $ h3_ [] "Android"

    a_ [href_ "ios.html"] $ h3_ [] "iOS"

    h3_ [] "Windows Phone"

    p_ [] $ do
      "La sincronizzazione è possibile con "
      strong_ [] "Google Calendar"
      "."
      "Seguire la guida "
      a_ [href_ "#android"] "sopra"
      " per importare il calendario ics in Google Calendar. Dopodiché seguire la seguente "
      a_ [href_ "http://www.mobileworld.it/2015/04/04/guida-sincronizzare-calendario-google-calendar-windows-phone-16726/"] "guida"
      " per sincronizzare Google Calendar con il calendario di Windows Phone. Buona fortuna!"

    h3_ [] "Sailfish OS"

    p_ [] $ do
      "Il calendario di sailfish ha un bug che per il momento non "
      "permette la visualizzazione della sezione \"altri calendari\" di google calendar."

    p_ [] $ do
      "Una possibile soluzione è ripiegare sul metodo di ICSdroid, e installare "
      a_ [href_ "https://f-droid.org/repository/browse/?fdid=org.sufficientlysecure.standalonecalendar"] "standalone-calendar"
      " per visualizzarlo."


iosPage :: Html ()
iosPage = wrapHtml (Just "iOS instructions") "" "" $ do
  h2_ [style_ "text-align:center;"] "Istruzioni per iOS"
  ol_ [] $ mapM_ (li_ [])
    [ "Copiare il link del calendario come descritto nella pagina principale;"
    , "Apri Impostazioni => Calendario => Aggiungi account => Altro => Aggiungi calendario;"
    , "Incolla l'indirizzo copiato precedentemente e conferma;"
    , "Apri il calendario: il gioco è fatto!" ]
  h2_ [style_ "text-align:center;"] "Screenshots:"
  table_ [] $
    mapM_
      (\n -> tr_ [] $ td_ [] $ img_ [class_ "screen", src_ ("screenshots/ios/" <> n <> ".jpg")])
      ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

androidPage :: Html ()
androidPage = wrapHtml (Just "Android instructions") "" "" $ do
  a_ [name_ "Android"] ""
  h3_ [] "Android"

  p_ [] "Se avete uno smartphone Android, molto probabilmente avrete anche l'account Google sincronizzato. Per esempio, se installate applicazioni dal Play Store o utilizzate Gmail, sicuramente lo è."

  h4_ [name_ "android"] "Android (con account Google sicronizzato)"

  p_ [] $ strong_ [] "Procedura online sul sito di Google (da fare la prima volta che si importa il calendario):"

  ol_ [] $ do
    li_ [] "Copiare il link del calendario come descritto sopra;"
    li_ [] $ do
      "Aprire il browser che si preferisce (Chrome, Firefox, eccetera) e collegarsi alla pagina: "
      a_ [href_ "https://google.com/calendar"] "google.com/calendar"
      ";"
    li_ [] "Fare il login su Google se necessario;"
    li_ [] "se appare l'interfaccia mobile, cliccare in fondo alla pagina sul link \"Visualizza: Desktop\";"
    li_ [] $ do
      "Sulla barra che vi si presenta a sinistra, sotto la voce "
      strong_ [] "Altri calendari"
      ", aprire il menu a tendina e cliccare su "
      strong_ [] "Aggiungi URL"
      ";"
    li_ [] $ do
      "Incollare il link (tasto destro => "
      strong_ [] "Incolla"
      ") e cliccare su "
      strong_ [] "Aggiungi calendario"
      ";"
    li_ [] $ do
      "Da smartphone, "
      strong_ [] "attivare la sincronizzazione del calendario"
      ":"
    li_ [] $ do
      "dalla propria applicazione calendario selezionare "
      strong_ [] "Calendari da visualizzare"
      ";"
    li_ [] $ do
      "selezionare e attivare il link che inizia per "
      em_ [] "http://unitn.fgaz.me/cal/..."
      "."
    li_ [] "Ritornare al calendario: il gioco è fatto!"

  h2_ [] "Screenshots:"
  table_ []
    (mapM_
      (\n -> tr_ [] $ td_ [] $ img_ [class_ "screen", src_ ("screenshots/android/" <> n <> ".png")])
      ["01", "02", "03", "04", "05", "06", "07"])

  h4_ [] "Android (senza account Google sincronizzato)"

  p_ [] "ICSdroid è una semplicissima applicazione che ci permette di sincronizzare il calendario fornendo un URL."
  p_ [] $ strong_ [] "Procedura mediante ICSdroid:"

  ol_ [] $ do
    li_ [] $ do
      "Copiare il link del calendario come descritto "
      a_ [href_ "instructions.html#copialink"] "sopra"
      ";"
    li_ [] $ do
      "Collegati a questa "
      a_ [href_ "https://f-droid.org/repository/browse/?fdid=at.bitfire.icsdroid"] "pagina"
      " e clicca su "
      strong_ [] "Download APK"
      ", o se preferisci scarica direttamente l'"
      a_ [href_ "https://f-droid.org/repo/at.bitfire.icsdroid_9.apk"] "apk"
      " (in alternativa l'applicazione è disponibile "
      a_ [href_ "https://play.google.com/store/apps/details?id=at.bitfire.icsdroid"] "sul play store"
      " a un piccolo prezzo);"
    li_ [] "Abilitare l'installazione di applicazioni da fonti sconosciute (<strong>Impostazioni</strong> =&gt; <strong>Sicurezza</strong> =&gt; <strong>Origini sconosciute</strong>);"
    li_ [] "Installare l'apk scaricato e aprire l'applicazione;"
    li_ [] "Aggiungi un calendario cliccando su <strong>+</strong>;"
    li_ [] "Incolla l'URL e dai la conferma;"
    li_ [] "Controlla sull'applicazione <strong>Calendario</strong> se la sincronizzazione è attiva;"
    li_ [] "Apri il calendario: il gioco è fatto!"


