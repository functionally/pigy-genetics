-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2021 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Command-line for image-token service.
--
-----------------------------------------------------------------------------


module Main (
-- * Service
  main
) where


import Cardano.Api        (ShelleyBasedEra(..))
import Mantis.Types       (runMantisToIO)
import Pigy.Chain         (runChain)
import Pigy.Types         (makeContext, readConfiguration)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)


-- | Run the service.
main :: IO ()
main =
  do
    [filename] <- getArgs
    result <-
      runMantisToIO
        $ do
            configuration <- readConfiguration filename
            context <- makeContext ShelleyBasedEraAlonzo configuration
            runChain context
    case result of
      Right () -> return ()
      Left e   -> hPutStrLn stderr e >> exitFailure
