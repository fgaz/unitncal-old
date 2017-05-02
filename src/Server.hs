{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Servant
import Data.IORef
import Data.Text
import Control.Monad.Except (liftIO)
import Data.Map as M
import Data.Maybe
import Data.Foldable as F
import Data.ByteString.Lazy (ByteString)

import Types
import Types.Servable
import ICal
import Network.HTTP.Media ((//), (/:))

--MAYBE use servant-lucid
data RenderedHTML

instance Accept RenderedHTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender RenderedHTML ByteString where
    mimeRender _ = id

unitncalApp :: FilePath -> IORef Servable -> Application
unitncalApp staticDir ref = serve unitncalAPI' $ unitncalServer' staticDir ref

unitncalAPI' :: Proxy UnitncalAPI'
unitncalAPI' = Proxy

unitncalAPI :: Proxy UnitncalAPI
unitncalAPI = Proxy

type UnitncalAPI' = UnitncalAPI :<|> Raw

type UnitncalAPI = "course" :> Capture "courseId" CourseId :> Capture "year" Year :> Get '[PlainText] Text
              :<|> "multical" :> QueryParams "c" SubjectId :> Get '[PlainText] Text
              :<|> "data.js" :> Get '[OctetStream] ByteString
              :<|> "index.html" :> Get '[RenderedHTML] ByteString
              :<|> Get '[RenderedHTML] ByteString

unitncalServer' :: FilePath -> IORef Servable -> Server UnitncalAPI'
unitncalServer' staticDir ref = unitncalServer ref
                           :<|> serveDirectoryWebApp staticDir

unitncalServer :: IORef Servable -> Server UnitncalAPI
unitncalServer ref = getCourse ref
                :<|> multical ref
                :<|> serveDirectly servableJs
                :<|> serveDirectly servableHtml
                :<|> serveDirectly servableHtml
  where serveDirectly f = f <$> liftIO (readIORef ref)

getCourse :: IORef Servable -> CourseId -> Year -> Handler Text
getCourse ref courseId year = do
  servable <- liftIO $ readIORef ref
  case M.lookup (courseId, year) $ servableCourses servable of Nothing -> throwError err404
                                                               Just iCal -> return iCal

multical :: IORef Servable -> [SubjectId] -> Handler Text
multical ref ss = do
  servable <- liftIO $ readIORef ref
  let cals = catMaybes $ flip M.lookup (servableClasses servable) <$> ss
  return $ wrapICal $ F.fold cals

