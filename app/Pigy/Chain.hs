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
-- | Monitor and act on the blockchain.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards    #-}


module Pigy.Chain (
-- * Running
  runChain
) where


<<<<<<< HEAD
import Cardano.Api                (AddressInEra, AssetId, BlockHeader(..), ChainPoint, IsCardanoEra, IsShelleyBasedEra, SlotNo(..), StakeAddressReference(NoStakeAddress), TxIn(..), TxOut(..), TxOutValue(..), Value, selectAsset)
=======
import Cardano.Api                (BlockHeader(..), ChainPoint, SlotNo(..), StakeAddressReference(NoStakeAddress), TxIn(..), TxOut(..), TxOutValue(..), Value, lovelaceToValue, selectAsset)
>>>>>>> b141755fccae69033c1196b2f58cf4d044d48cb0
import Control.Lens               ((.~), (%~))
import Control.Monad              (unless, when)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState(..), modify)
import Data.Default               (Default(..))
import Data.IORef                 (IORef, newIORef)
import Data.Maybe                 (mapMaybe)
import Mantis.Chain               (watchTransactions)
import Mantis.Script              (mintingScript)
import Mantis.Types               (MantisM, runMantisToIO)
import Mantis.Transaction         (printValueIO)
import Mantis.Wallet              (showAddressInEra, stakeReferenceInEra)
import Pigy.Chain.Mint            (checkValue, mint)
import Pigy.Chain.Types           (Chain, ChainState(..), History, Origins, Pendings, activeLens, currentLens, historyLens, originsLens, pendingsLens, redosLens, undosLens, withChainState)
import Mantis.Wallet              (showAddressMary, stakeReferenceMary)
import Pigy.Chain.Mint            (checkValue, findPigs, mint)
import Pigy.Chain.Types           (Chain, ChainState(..), History, MaryAddress, Origins, Pendings, activeLens, currentLens, historyLens, originsLens, pendingsLens, redosLens, undosLens, withChainState)
import Pigy.Types                 (Context(..), KeyedAddress(..), Mode(..))

import qualified Data.Map.Strict as M (delete, difference, fromListWith, insert, keysSet, lookup, member, size, toList)
import qualified Data.Set        as S (delete, member, union)


-- | Record history.
record :: Int                 -- ^ The number of rollbacks to allow.
       -> SlotNo              -- ^ The curent slot number.
       -> (Origins, Pendings) -- ^ The tracked transactions and queued mintings.
       -> History             -- ^ The original history.
       -> History             -- ^ The augmented history.
record rollbacks slot sourcePending = take rollbacks . ((slot, sourcePending) :)


-- | Roll back history.
rollback :: SlotNo      -- ^ The slot number to revert to.
         -> History era -- ^ The original history.
         -> History era -- ^ The rolled-back history.
rollback slot = dropWhile $ (/= slot) . fst


-- | Extract the slot number from the chain point.
toSlotNo :: ChainPoint -- ^ The chain point.
         -> SlotNo     -- ^ The slot number.
toSlotNo point =
  -- FIXME: Find a less fragile way to extract the slot number at a chain point.
  case show point of
    "ChainPointAtGenesis" -> SlotNo 0
    text                  -> SlotNo . read . takeWhile (/= ')') $ drop 19 text


-- | Allow minting.
makeActive :: Chain era () -- ^ Action to modify the chain state.
makeActive =
  do
    ChainState{..} <- get
    unless active
      $ do
        liftIO
          $ do
            putStrLn ""
            putStrLn "First idling."
        modify
          $ activeLens .~ True


-- | Record a new block.
recordBlock :: SlotNo       -- ^ The slot number.
            -> Chain era () -- ^ Action to modify the chain state.
recordBlock slot =
  do
    ChainState{..} <- get
    when (verbose context && active)
      . liftIO
      $ do
        putStrLn ""
        putStrLn $ "New block: " ++ show current ++ " -> " ++ show slot
        putStrLn $ "  Origins:  " ++ show (M.size origins )
        putStrLn $ "  Pendings: " ++ show (M.size pendings)
    modify
      $ (currentLens .~ slot)
      . (historyLens %~ record (kSecurity context) current (origins, pendings))


-- | Roll back the chain state.
recordRollback :: IsCardanoEra era
               => SlotNo       -- ^ The slot number to roll back to.
               -> Chain era () -- ^ The action to modify the chain state.
recordRollback slot =
  do
    ChainState{..} <- get
    let
      history'@((_, (origins', pendings')) : _) = rollback slot history
    liftIO
      $ do
        putStrLn ""
        putStrLn $ "Rollback: " ++ show slot ++ " <- " ++ show current
    printRollback (origins, origins') (pendings, pendings')
    modify
      $ (currentLens  .~ slot                                                  )
      . (originsLens  .~ origins'                                              )
      . (pendingsLens .~ pendings'                                             )
      . (historyLens  .~ history'                                              )
      . (undosLens    %~ S.union (M.keysSet $ origins  `M.difference` origins'))
      . (redosLens    %~ S.union (M.keysSet $ origins' `M.difference` origins ))


-- | Record the input to a transaction.
recordInput :: SlotNo       -- ^ The slot number.
            -> TxIn         -- ^ The spent UTxO.
            -> Chain era () -- ^ The action to modify the chain state.
recordInput slot txIn =
  do
    ChainState{..} <- get
    when (txIn `S.member` redos)
      $ do
        liftIO
          $ do
            putStrLn ""
            putStrLn $ "Re-spending rolled-back spending: " ++ show txIn
        modify
          $ redosLens %~ S.delete txIn
    let
      found     = txIn `M.member` origins
      isPending = txIn `M.member` pendings
    when found
      $ do
        when (verbose context || isPending)
          . liftIO
          $ do
            putStrLn ""
            putStrLn $ show slot ++ ": spent " ++ show txIn
        modify
          $ (originsLens  %~ M.delete txIn)
          . (pendingsLens %~ M.delete txIn)


-- | Record the output of a transaction.
recordOutput :: IsCardanoEra era
             => IsShelleyBasedEra era
             => [TxIn]           -- ^ The spend UTxOs.
             -> TxIn             -- ^ The UTxO.
             -> AddressInEra era -- ^ The destination address.
             -> Value            -- ^ The total value.
             -> Chain era ()     -- ^ The action to modify the chain state.
recordOutput inputs output destination value =
  do
    ChainState{..} <- get
    when (output `S.member` undos)
      $ do
        liftIO
          $ do
            putStrLn ""
            putStrLn $ "Re-transacting rolled-back transaction: " ++ show output
        modify
          $ undosLens %~ S.delete output
    modify
      $ originsLens %~ M.insert output destination
    let
      sources = mapMaybe (`M.lookup` origins) inputs
      valid = checker value
    when (verbose context || destination == scriptAddress)
      . liftIO
      $ do
        putStrLn ""
        putStrLn $ "Output: " ++ show output
        sequence_
          [
            do
              putStrLn $ "  Source: " ++ show (showAddressInEra source')
              putStrLn $ "    Stake: " ++ show (stakeReferenceInEra source')
          |
            source' <- sources
          ]
        putStrLn $ "  Destination: " ++ showAddressInEra destination
        putStrLn $ "  To me: " ++ show (destination == scriptAddress)
        putStrLn $ "  Valid: " ++ show valid
        printValueIO "  " value
    when (destination == scriptAddress)
      $ if active && valid && operation context == Aggressive
          then createToken [output] (head sources, value)
          else do
                 liftIO $ putStrLn "  Queued for creation."
                 modify
                   $ pendingsLens %~ M.insert output (sources, value)


-- | Mint a token from a single transaction.
createPendingSingle :: IsShelleyBasedEra era
                    => Chain era () -- ^ The action to modify the chain state.
createPendingSingle =
  do
    ChainState{..} <- get
    sequence_
      [
        createToken [output] (head sources, value)
      |
        (output, (sources, value)) <- M.toList pendings
      , checker value
      ]


-- | Mint a token from multiple transactions.
createPendingMultiple :: IsCardanoEra era
                      => IsShelleyBasedEra era
                      => Chain era ()-- ^ The action to modify the chain state.
createPendingMultiple =
  do
    ChainState{..} <- get
    sequence_
      [
        do
          when (verbose context || valid)
            . liftIO
            $ do
              putStrLn ""
              putStrLn "Processing input transactions:"
              putStrLn $ "  Stake: " ++ stake
              sequence_
                [
                  putStrLn $ "  Source: " ++ show (showAddressInEra source)
                |
                  source <- sources
                ]
              putStrLn $ "  Valid: " ++ show valid
              printValueIO "  " value
          when valid
            $ createToken outputs (head sources, value)
      |
        let pending' = M.fromListWith
                         (
                           \(outputs, sources, value) (outputs', sources', value') ->
                             (
                               outputs <> outputs'
                             , sources <> sources'
                             , value   <> value'
                             )
                         )
                         . map
                         (
                           \(output, (sources, value)) ->
                             (
                               show . stakeReferenceInEra $ head sources
                             , (
                                 [output]
                               , sources
                               , value
                               )
                             )
                         )
                         $ M.toList pendings
      , (stake, (outputs, sources, value)) <- M.toList pending'
      , let valid = checker value
      , stake /= show NoStakeAddress
      ]


-- | Mint or burn a token.
createToken :: IsCardanoEra era
            => IsShelleyBasedEra era
            => [TxIn]                    -- ^ The UTxOs to spend.
            -> (AddressInEra era, Value) -- ^ The destination and total value.
            -> Chain era ()              -- ^ The action to modify the chain state.
createToken inputs (destination, value) =
  do
    ChainState{..} <- get
    liftIO
      $ do
        putStrLn ""
        putStrLn "Minting token."
        sequence_
          [
            putStrLn $ "  Input: " ++ show input
          |
            input <- inputs
          ]
        putStrLn $ "  Destination: " ++ showAddressInEra destination
        putStrLn $ "  To me: "++ show (destination == scriptAddress)
        printValueIO "  " value
    sequence_
      [
        -- FIXME: Consider deleting the transactions only if the minting succeeeds.
        modify
          $ (originsLens  %~ M.delete input)
          . (pendingsLens %~ M.delete input)
      |
        input <-inputs
      ]
    unless (destination == scriptAddress)
      $ liftIO $ do
        let
          message =
            case (selectAsset value $ token context, length inputs) of
              (1, 1) -> [
                          "Thank you!"
                        ]
              (1, _) -> [
                          "Please send the tokens and ADA in a single transaction."
                        ]
              (_, 1) -> [
                          "There is a limit of one minting per transaction."
                        , "Sending more that one PIGY does not mint more pig images."
                        ]
              (_, _) -> [
                          "There is a limit of one minting per transaction."
                        , "Sending more that one PIGY does not mint more pig images."
                        , "Also, please send the tokens and ADA in a single transaction."
                        ]
        result <- runMantisToIO $ mint context inputs destination value message
        either print return result


-- | Print diagnostic information for a rollback.
printRollback :: IsCardanoEra era
              => (Origins  era, Origins  era) -- ^ The prior and posterior tracking of transaction origins.
              -> (Pendings era, Pendings era) -- ^ The prior and posterior queues for minting.
              -> Chain era ()                 -- ^ The action to modify the chain state.
printRollback (origins, origins') (pendings, pendings') =
  do
    unless (origins == origins')
      . liftIO
      $ do
        putStrLn "  Origins:"
        printOrigins "Removed by rollback:"
          $ origins `M.difference` origins'
        printOrigins "Added by rollback:"
          $ origins' `M.difference` origins
    unless (pendings == pendings')
      . liftIO
      $ do
        printPendings "Removed by rollback:"
          $ pendings `M.difference` pendings'
        putStrLn "  Pendings:"
        printPendings "Added by rollback:"
          $ pendings' `M.difference` pendings


-- | Print diagnostic information for transaction origins.
printOrigins :: IsCardanoEra era
             => String      -- ^ The prefatory message.
             -> Origins era -- ^ The transaction origins.
             -> IO ()       -- ^ The action to print the information.
printOrigins message origins' =
  do
    putStrLn $ "    " ++ message
    sequence_
      [
        do
          putStrLn $ "      " ++ show output
          putStrLn $ "        Source: " ++ show (showAddressInEra source)
      |
        (output, source) <- M.toList origins'
      ]


-- | Print diagnostic information for queued mintings.
printPendings :: IsCardanoEra era
              => String       -- ^ The prefatory message.
              -> Pendings era -- ^ The queued mintings.
              -> IO ()        -- ^ The action to print the information.
printPendings message pendings' =
  do
    putStrLn $ "    " ++ message
    sequence_
      [
        do
          putStrLn $ "      " ++ show output
          sequence_
            [
              putStrLn $ "        Source: " ++ show (showAddressInEra source)
            |
              source <- sources
            ]
          printValueIO "        " value
      |
        (output, (sources, value)) <- M.toList pendings'
      ]


-- | Run the chain operations for tracking and minting.
runChain :: IsCardanoEra era
         => IsShelleyBasedEra era
         => MonadFail m
         => MonadIO m
         => Context era  -- ^ The service context.
         -> MantisM m () -- ^ Action to run the operations.
runChain context@Context{..} =
  do
    let
      KeyedAddress{..} = keyedAddress
      (script, scriptHash) = mintingScript verificationHash Nothing
    chainState <-
      liftIO
        . newIORef
        $ def
          {
            context       = context
          , scriptAddress = keyAddress
          , script        = script
          , scriptHash    = scriptHash
          , checker       = checkValue token scriptHash
          }
    let
      blockHandler (BlockHeader slot _ _) _ =
        withChainState chainState
          $ recordBlock slot
      rollbackHandler point _ =
        withChainState chainState
          . recordRollback
          $ toSlotNo point
      idleHandler =
        withChainState chainState
          $ do
            makeActive
            unless (operation == Lenient)
              createPendingSingle
            unless (operation == Strict)
              createPendingMultiple
            return False
      inHandler (BlockHeader slot _ _) txIn =
        withChainState chainState
          $ recordInput slot txIn
      outHandler _ inputs output (TxOut destination txOutValue) =
        let
          value =
            case txOutValue of
              TxOutValue   _ value'   -> value'
              TxOutAdaOnly _ lovelace -> lovelaceToValue lovelace
          hasToken = selectAsset value token > 0
          hasImage = not . null $ findPigs scriptHash value
        in
          when (hasToken || hasImage || destination == keyAddress)
            . withChainState chainState
            $ recordOutput inputs output destination value
    watchTransactions
      socket
      protocol
      network
      (Just rollbackHandler)
      idleHandler
      blockHandler
      inHandler
      outHandler
