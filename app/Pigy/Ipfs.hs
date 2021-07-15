
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


module Pigy.Ipfs (
  pinImage
) where


import Control.Monad.IO.Class    (MonadIO)
import Development.Shake.Command (CmdOption(Env), Exit(..), Stderr(..), Stdout(..), cmd)
import Mantis.Types              (MantisM, foistMantisEitherIO)
import Pigy.Image                (Genotype, toChromosome, writeImage)
import Pigy.Image.Types          (Chromosome, Phenable(..))
import System.FilePath.Posix     ((</>), (<.>))
import System.Exit               (ExitCode(..))


pinImage :: MonadIO m
         => (String, [(String, String)])
         -> FilePath
         -> Genotype
         -> MantisM m (Chromosome, String)
pinImage (remote, environment) folder genotype =
  foistMantisEitherIO
    $ do
      let
        chromosome = toChromosome genotype
        filename = folder </> "PIG@" <> chromosome <.> "png"
      writeImage filename $ toPhenotype genotype
      (Exit code, Stdout cid', Stderr msg) <-
        cmd
          [Env environment]
          "ipfs"
          ["add", "--quieter", "--pin=false", filename]
      case code of
        ExitFailure _ -> return $ Left (msg :: String)
        ExitSuccess   -> do
                           let cid = init cid'
                           (Exit code', Stdout result, Stderr msg') <-
                             cmd
                               [Env environment]
                               "ipfs"
                               ["pin", "remote", "add", "--service=" ++ remote, "--name=PIG@" ++ chromosome, cid]
                           case code' of
                             ExitFailure _ -> return $ Left (msg' :: String)
                             ExitSuccess   -> do
                                                sequence_
                                                  [
                                                    putStrLn $ "  " ++ line
                                                  |
                                                    line <- lines result
                                                  ]
                                                return $ Right (chromosome, cid)
