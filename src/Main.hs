module Main where

import Generator
import Server
import Network.Wai.Handler.Warp
import Data.IORef

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Monoid
import Data.Default.Class
import Types
import qualified Config
import Paths_unitncal


main :: IO ()
main = do
  putStrLn "Scraping and generation of the initial servable is deferred."
  ref <- newIORef def
  putStrLn "Forking periodic scraper..."
  _ <- forkIO $ threadDelay Config.refreshInterval *> every Config.refreshInterval (updateServable ref)
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
  newServable <- makeNewServable
  oldServable <- readIORef ref
  let servable = mergeServables oldServable newServable
  writeIORef ref servable

