module AppMain where

import System.Environment (getArgs)
import qualified Main
import qualified MainGUI

-- | Main entrypoint for the application
main :: IO ()
main = do
  args <- getArgs
  
  -- Check if there are command-line arguments
  case args of
    ["--cli"] -> Main.main       -- CLI version
    ["--help"] -> showHelp
    _ -> MainGUI.mainGUI         -- Default to GUI version

-- | Show help information
showHelp :: IO ()
showHelp = do
  putStrLn "Just Intonation Music Generator"
  putStrLn "==============================="
  putStrLn "Usage:"
  putStrLn "  euterpea2-project           - Launch graphical interface (default)"
  putStrLn "  euterpea2-project --cli     - Launch command-line interface"
  putStrLn "  euterpea2-project --help    - Show this help message"