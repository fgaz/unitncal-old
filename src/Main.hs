module Main where

import Generator
import Server
import Network.Wai.Handler.Warp
import Data.IORef

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Monoid
import Types
import qualified Config

main :: IO ()
main = do
  putStrLn "Scraping and generating initial servable..."
  servable <- makeNewServable
  ref <- newIORef servable
  putStrLn "Done."
  putStrLn "Forking periodic scraper..."
  _ <- forkIO $ every Config.refreshInterval $ updateServable ref
  putStrLn "Done."
  putStrLn "Running server..."
  runSettings (setPort Config.port $ setHost Config.host defaultSettings) $ unitncalApp ref

every :: Int -> IO a -> IO b
every n f = forever (void f <> threadDelay n)

mergeServables :: Servable -> Servable -> Servable
mergeServables _ = id

updateServable :: IORef Servable -> IO ()
updateServable ref = do
  newServable <- makeNewServable
  oldServable <- readIORef ref
  let servable = mergeServables oldServable newServable
  writeIORef ref servable

