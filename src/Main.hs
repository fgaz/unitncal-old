{-# LANGUAGE OverloadedStrings #-}
module Main where

import Generator
import Server
import Network.Wai.Handler.Warp
import Data.IORef

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Monoid
import Types

main :: IO ()
main = do
  putStrLn "Scraping and generating initial servable..."
  servable <- makeNewServable
  ref <- newIORef servable
  putStrLn "Done."
  putStrLn "Forking periodic scraper..."
  _ <- forkIO $ every (60*60*1000000) $ updateServable ref
  putStrLn "Done."
  putStrLn "Running server..."
  runSettings (setPort 8080 $ setHost "127.0.0.1" defaultSettings) $ unitncalApp ref

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

