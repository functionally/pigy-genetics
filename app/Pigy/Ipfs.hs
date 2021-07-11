
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


module Pigy.Ipfs (
  pinImage
) where


import Pigy.Image            (Chromosome, Genotype, toChromosome, toPhenotype, writeImage)
import System.Command        (CmdOption(Env), Stdout(..), command)
import System.FilePath.Posix ((</>), (<.>))


pinImage :: (String, [(String, String)])
         -> FilePath
         -> Genotype
         -> IO (Chromosome, String)
pinImage (remote, environment) folder genotype =
  do
    -- FIXME: Catch errors in `MantisM`.
    let
      chromosome = toChromosome genotype
      filename = folder </> "PIG@" <> chromosome <.> "png"
    writeImage filename $ toPhenotype genotype
    cid <-
      init . fromStdout
        <$> command
            [Env environment]
            "ipfs"
            ["add", "--quieter", "--pin=false", filename]
    result <-
      init . fromStdout
        <$> command
        [Env environment]
        "ipfs"
        ["pin", "remote", "add", "--service=" ++ remote, "--name=PIG@" ++ chromosome, cid]
    sequence_
      [
        putStrLn $ "  " ++ line
      |
        line <- lines result
      ]
    return (chromosome, cid)
    
