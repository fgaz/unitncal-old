{-# LANGUAGE OverloadedStrings #-}
module Main where

import Generator
import Server
import Network.Wai.Handler.Warp
import Data.IORef

main :: IO ()
main = do
  putStrLn "Scraping and generating initial servable..."
  servable <- makeNewServable
  putStrLn "Done."
  -- TODO fork a periodic scraper
  putStrLn "Running server..."
  ref <- newIORef servable
  runSettings (setPort 8080 $ setHost "127.0.0.1" defaultSettings) $ unitncalApp ref

