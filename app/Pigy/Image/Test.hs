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
-- | Test utilities for pig images.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}


module Pigy.Image.Test (
-- * Testing
  test
, testCreate
, testCrossover
, testTree
, testVersion
) where


import Control.Monad          (replicateM)
import Pigy.Image             (Genotype(..), crossover, fromChromosome, toChromosome, writeImage)
import Pigy.Image.Types       (Phenable(..), Upgradeable(..))
import System.Random          (getStdGen)
import System.Random.Internal (uniformM)
import System.Random.Stateful (StatefulGen, newIOGenM)


-- | Run all tests.
test :: IO ()
test =
  do
    g <- newIOGenM =<< getStdGen
    testCreate g
    testCrossover g
    testVersion g
    testTree g


-- | Test image creation.
testCreate :: StatefulGen g IO
           => g     -- ^ The random-number generator.
           -> IO () -- ^ Action for running the test.
testCreate g =
  do
    genotype <- uniformM g
    let
      chromosome = toChromosome genotype
      Just genotype' = fromChromosome chromosome
      phenotype = toPhenotype genotype'
      Just genotype'' = fromChromosome $ toChromosome genotype'
    writeImage ("pigy-" ++ chromosome ++ ".png") phenotype
    putStrLn ""
    putStrLn $ "Chromosome: " ++ chromosome
    putStrLn ""
    putStrLn $ "Before encoding: " ++ show genotype
    putStrLn ""
    putStrLn $ "After encoding: " ++ show genotype'
    putStrLn ""
    putStrLn $ "Encoding okay: " ++ show (genotype' == genotype'')


-- | Test crossover.
testCrossover :: StatefulGen g IO
              => g     -- ^ The random-number generator.
              -> IO () -- ^ Action for running the test.
testCrossover g =
  do
    parent  <- uniformM g
    parent' <- uniformM g
    offspring <- crossover g [parent, parent']
    putStrLn ""
    putStrLn $ "Parent 1: " ++ show parent
    putStrLn ""
    putStrLn $ "Parent 2: " ++ show parent'
    putStrLn ""
    putStrLn $ "Offsping: " ++ show offspring
    writeImage "pigy-parent-1.png"
      $ toPhenotype parent
    writeImage "pigy-parent-2.png"
      $ toPhenotype parent'
    writeImage "pigy-offspring.png"
      $ toPhenotype offspring


-- | Test versioning.
testVersion :: StatefulGen g IO
            => g     -- ^ The random-number generator.
            -> IO () -- ^ Action for running the test.
testVersion g =
  do
    genotype <- GenotypeV0 <$> uniformM g
    let
      chromosome = toChromosome genotype
      Just genotype' = fromChromosome chromosome
      phenotype = toPhenotype genotype'
      Just genotype'' = fromChromosome $ toChromosome genotype'
    writeImage ("pigy-" ++ chromosome ++ ".png") phenotype
    putStrLn ""
    putStrLn $ "Chromosome: " ++ chromosome
    putStrLn ""
    putStrLn $ "Before encoding: " ++ show genotype
    putStrLn ""
    putStrLn $ "After encoding: " ++ show genotype'
    putStrLn ""
    putStrLn $ "After upgrade: " ++ show (upgrade genotype' :: Genotype)
    putStrLn ""
    putStrLn $ "Encoding okay: " ++ show (genotype' == genotype'')
    parent  <- GenotypeV0 <$> uniformM g
    parent' <- uniformM g
    offspring <- crossover g [parent, parent']
    putStrLn ""
    putStrLn $ "Parent 1: " ++ show parent
    putStrLn ""
    putStrLn $ "Parent 2: " ++ show parent'
    putStrLn ""
    putStrLn $ "Offsping: " ++ show offspring
    writeImage "pigy-parent-1.png"
      $ toPhenotype parent
    writeImage "pigy-parent-2.png"
      $ toPhenotype parent'
    writeImage "pigy-offspring.png"
      $ toPhenotype offspring


-- | Test family trees.
testTree :: StatefulGen g IO
         => g     -- ^ The random-number generator.
         -> IO () -- ^ Action for running the test.
testTree g =
  do
    putStrLn "digraph pigy {"
    parents <- replicateM 6 (uniformM g)
    sequence_
      [
        do
          putStrLn $ "P_" ++ tag ++ " [label=\"" ++ tag ++ "\" labelloc=\"t\" shape=box image=\"" ++ filename ++ "\"]"
          writeImage filename $ toPhenotype parent
      |
        parent <- parents
      , let tag      =  toChromosome parent
            filename = "pigy-" ++ tag ++ ".png"
      ]
    children <-
      sequence
        [
          do
            child <- crossover g [parent1, parent2]
            let
              tag      =  toChromosome child
              filename = "pigy-" ++ tag ++ ".png"
            putStrLn $ "P_" ++ tag ++ " [label=\"" ++ tag ++ "\" labelloc=\"t\" shape=box image=\"" ++ filename ++ "\"]"
            putStrLn $ "P_" ++ tag1 ++ " -> " ++ "P_" ++ tag
            putStrLn $ "P_" ++ tag2 ++ " -> " ++ "P_" ++ tag
            writeImage filename $ toPhenotype child
            return child
        |
          (parent1, parent2) <- zip (init parents) (tail parents)
        , let
            tag1 =  toChromosome parent1
            tag2 =  toChromosome parent2
        ]
    grandchildren <-
      sequence
        [
          do
            child <- crossover g [parent1, parent2]
            let
              tag      =  toChromosome child
              filename = "pigy-" ++ tag ++ ".png"
            putStrLn $ "P_" ++ tag ++ " [label=\"" ++ tag ++ "\" labelloc=\"t\" shape=box image=\"" ++ filename ++ "\"]"
            putStrLn $ "P_" ++ tag1 ++ " -> " ++ "P_" ++ tag
            putStrLn $ "P_" ++ tag2 ++ " -> " ++ "P_" ++ tag
            writeImage filename $ toPhenotype child
            return child
        |
          (parent1, parent2) <- zip (init children) (tail children)
        , let
            tag1 =  toChromosome parent1
            tag2 =  toChromosome parent2
        ]
    sequence_
      [
        do
          child <- crossover g [parent1, parent2]
          let
            tag      =  toChromosome child
            filename = "pigy-" ++ tag ++ ".png"
          putStrLn $ "P_" ++ tag ++ " [label=\"" ++ tag ++ "\" labelloc=\"t\" shape=box image=\"" ++ filename ++ "\"]"
          putStrLn $ "P_" ++ tag1 ++ " -> " ++ "P_" ++ tag
          putStrLn $ "P_" ++ tag2 ++ " -> " ++ "P_" ++ tag
          writeImage filename $ toPhenotype child
          return child
      |
        (parent1, parent2) <- zip (init grandchildren) (tail grandchildren)
      , let
          tag1 =  toChromosome parent1
          tag2 =  toChromosome parent2
      ]
    putStrLn "}"
