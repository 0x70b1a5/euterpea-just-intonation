module Main where

import TrackerWeb
import ErrorHandler

main :: IO ()
main = withErrorHandling $ do
  putStrLn "Starting web-based tracker interface..."
  startWebTracker Nothing