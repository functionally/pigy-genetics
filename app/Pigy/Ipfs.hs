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
-- | IPFS pinning for pig images.
--
-----------------------------------------------------------------------------


module Pigy.Ipfs (
-- * IPFS
  pinImage
) where


import Control.Monad.IO.Class    (MonadIO)
import Development.Shake.Command (Exit(..), Stderr(..), Stdout(..), cmd)
import Mantra.Types              (MantraM, foistMantraEitherIO)
import Pigy.Image                (Genotype, toChromosome, writeImage)
import Pigy.Image.Types          (Chromosome, Phenable(..))
import System.FilePath.Posix     ((</>), (<.>))
import System.Exit               (ExitCode(..))


-- | Pin an image to IPFS.
pinImage :: MonadFail m
         => MonadIO m
         => FilePath                       -- ^ The IPFS shell script.
         -> FilePath                       -- ^ The folder for images.
         -> Genotype                       -- ^ The genotype of the image.
         -> MantraM m (Chromosome, String) -- ^ Action for pinning the image and returning its chromosome and IPFS CID.
pinImage script folder genotype =
  foistMantraEitherIO
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
