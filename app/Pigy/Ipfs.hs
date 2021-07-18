
module Pigy.Ipfs (
  pinImage
) where


import Control.Monad.IO.Class    (MonadIO)
import Development.Shake.Command (Exit(..), Stderr(..), Stdout(..), cmd)
import Mantis.Types              (MantisM, foistMantisEitherIO)
import Pigy.Image                (Genotype, toChromosome, writeImage)
import Pigy.Image.Types          (Chromosome, Phenable(..))
import System.FilePath.Posix     ((</>), (<.>))
import System.Exit               (ExitCode(..))


pinImage :: MonadFail m
         => MonadIO m
         => FilePath
         -> FilePath
         -> Genotype
         -> MantisM m (Chromosome, String)
pinImage script folder genotype =
  foistMantisEitherIO
    $ do
      let
        chromosome = toChromosome genotype
        filename = folder </> "PIG@" <> chromosome <.> "png"
      writeImage filename $ toPhenotype genotype
      (Exit code, Stdout result, Stderr msg) <-
        cmd
          script
          ["PIG@" ++ chromosome, filename]
      let
        cid : message = lines result
      case code of
        ExitFailure _ -> return $ Left (msg :: String)
        ExitSuccess   -> do
                           sequence_
                             [
                               putStrLn $ "  " ++ line
                             |
                               line <- message
                             ]
                           return $ Right (chromosome, cid)
