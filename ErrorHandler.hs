-- |
-- Module      : ErrorHandler
-- Description : Error handling utilities for the Just Intonation application
-- 
-- This module provides functionality to catch and log exceptions to file.

{-# LANGUAGE ScopedTypeVariables #-}

module ErrorHandler 
  ( withErrorHandling
  , logException
  ) where

import Control.Exception (catch, SomeException, displayException, Handler(..), catches)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.IO (openFile, hPutStrLn, hClose, IOMode(AppendMode))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Data.Typeable (Typeable, cast)
import Data.List (isInfixOf)

-- | File path for logs
logFilePath :: IO FilePath
logFilePath = return "crash.log"

-- | Log an exception with timestamp to the log file
logException :: SomeException -> IO ()
logException e = do
  logPath <- logFilePath
  timestamp <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  let message = formattedTime ++ " - ERROR: " ++ displayException e
  
  -- Create parent directory if needed
  fileExists <- doesFileExist logPath
  if not fileExists
    then do
      -- Touch the file
      writeFile logPath ""
    else return ()
  
  -- Append the error to the log file
  handle <- openFile logPath AppendMode
  hPutStrLn handle message
  hClose handle
  
  -- Print message to console
  putStrLn "An error occurred. Details have been saved to crash.log."
  putStrLn "Please try restarting the application."

-- | Check if exception is a browser disconnection error
isBrowserDisconnectError :: SomeException -> Bool
isBrowserDisconnectError e = 
  let msg = displayException e
  in "Browser window disconnected" `isInfixOf` msg ||
     "Foreign.JavaScript: Browser window disconnected" `isInfixOf` msg

-- | Run an IO action with error handling and logging
withErrorHandling :: IO a -> IO a
withErrorHandling action = action `catch` \(e :: SomeException) -> 
  if isBrowserDisconnectError e
  then do
    putStrLn "Browser window disconnected. This is normal behavior and not a crash."
    putStrLn "The application is still running. Please reconnect by refreshing your browser."
    putStrLn "If problems persist, try restarting the application."
    -- Continue running despite the disconnect
    action
  else do
    logException e
    error "Application crashed. See crash.log for details."

