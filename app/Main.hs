module Main (
  main
) where


import Mantis.Types       (runMantisToIO)
import Pigy.Chain         (runChain)
import Pigy.Types         (makeContext, readConfiguration)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)


main :: IO ()
main =
  do
    [filename] <- getArgs
    result <-
      runMantisToIO
        $ do
            configuration <- readConfiguration filename
            context <- makeContext configuration
            runChain context
    case result of
      Right () -> return ()
      Left e   -> hPutStrLn stderr e >> exitFailure
