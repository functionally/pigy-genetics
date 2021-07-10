module Main (
  main
) where


import Mantis.Types (runMantisToIO)
import Pigy.Chain   (pigy)
import Pigy.Types   (makeContext, readConfiguration)
import System.Exit  (exitFailure)
import System.IO    (hPutStrLn, stderr)


main :: IO ()
main =
  do
    result <-
      runMantisToIO
        $ do
            configuration <- readConfiguration "testnet.pigy"
            context <- makeContext configuration
            pigy context
    case result of
      Right () -> return ()
      Left e   -> hPutStrLn stderr e >> exitFailure
