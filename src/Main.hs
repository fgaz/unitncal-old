module Main where

import Generator
import Server
import Network.Wai.Handler.Warp
import Data.IORef

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Monoid
import Data.Default.Class
import Types.Servable
import qualified Config
import Paths_unitncal


main :: IO ()
main = do
  putStrLn "Scraping and generation of the initial servable is deferred."
  ref <- newIORef def
  putStrLn "Forking periodic scraper..."
  _ <- forkIO $ every Config.refreshInterval $ updateServable ref
  putStrLn "Done."
  putStrLn "Running server..."
  dataDir <- getDataDir
  let staticDir = dataDir <> "/static"
  runSettings (setPort Config.port $ setHost Config.host defaultSettings) $ unitncalApp staticDir ref

every :: Int -> IO a -> IO b
every n f = forever (f *> threadDelay n)

mergeServables :: Servable -> Servable -> Servable
mergeServables _ = id

updateServable :: IORef Servable -> IO ()
updateServable ref = do
  newServable' <- makeNewServable
  case newServable'
    of Left err -> print err
       Right newServable ->
         do
           oldServable <- readIORef ref
           -- TODO we should get a [Either Error Servable]
           -- (or more probably ([Error], Servable)),
           -- log the errors and
           -- actually merge the servables
           let servable = mergeServables oldServable newServable
           writeIORef ref servable

