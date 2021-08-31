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


{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}


module Pigy.Chain (
-- * Running
  runChain
) where


import Cardano.Api                (AddressAny, AddressInEra(..), AddressTypeInEra(..), BlockHeader(..), ChainPoint(..), SlotNo(..), StakeAddressReference(NoStakeAddress), TxIn(..), TxOut(..), TxOutValue(..), Value, lovelaceToValue, selectAsset, toAddressAny)
import Control.Lens               ((.~), (%~))
import Control.Monad              (unless, when)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState(..), modify)
import Data.Default               (Default(..))
import Data.IORef                 (newIORef)
import Data.Maybe                 (mapMaybe)
import Mantra.Chain               (watchTransactions)
import Mantra.Script              (mintingScript)
import Mantra.Types               (MantraM, runMantraToIO)
import Mantra.Transaction         (printValueIO)
import Mantra.Wallet              (showAddress, stakeReference)
import Pigy.Chain.Mint            (checkValue, findPigs, mint)
import Pigy.Chain.Types           (Chain, ChainState(..), History, Origins, Pendings, activeLens, currentLens, historyLens, originsLens, pendingsLens, redosLens, undosLens, withChainState)
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
rollback :: SlotNo  -- ^ The slot number to revert to.
         -> History -- ^ The original history.
         -> History -- ^ The rolled-back history.
rollback slot = dropWhile $ (/= slot) . fst


-- | Extract the slot number from the chain point.
toSlotNo :: ChainPoint -- ^ The chain point.
         -> SlotNo     -- ^ The slot number.
toSlotNo ChainPointAtGenesis = SlotNo 0
toSlotNo (ChainPoint slot _) = slot


-- | Allow minting.
makeActive :: Chain () -- ^ Action to modify the chain state.
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
recordBlock :: SlotNo   -- ^ The slot number.
            -> Chain () -- ^ Action to modify the chain state.
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
recordRollback :: SlotNo   -- ^ The slot number to roll back to.
               -> Chain () -- ^ The action to modify the chain state.
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
      $ (activeLens   .~ False                                                 )
      . (currentLens  .~ slot                                                  )
      . (originsLens  .~ origins'                                              )
      . (pendingsLens .~ pendings'                                             )
      . (historyLens  .~ history'                                              )
      . (undosLens    %~ S.union (M.keysSet $ origins  `M.difference` origins'))
      . (redosLens    %~ S.union (M.keysSet $ origins' `M.difference` origins ))


-- | Record the input to a transaction.
recordInput :: SlotNo   -- ^ The slot number.
            -> TxIn     -- ^ The spent UTxO.
            -> Chain () -- ^ The action to modify the chain state.
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
recordOutput :: [TxIn]     -- ^ The spend UTxOs.
             -> TxIn       -- ^ The UTxO.
             -> AddressAny -- ^ The destination address.
             -> Value      -- ^ The total value.
             -> Chain ()   -- ^ The action to modify the chain state.
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
              putStrLn $ "  Source: " ++ show (showAddress source')
              putStrLn $ "    Stake: " ++ show (stakeReference source')
          |
            source' <- sources
          ]
        putStrLn $ "  Destination: " ++ showAddress destination
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
createPendingSingle :: Chain () -- ^ The action to modify the chain state.
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
createPendingMultiple :: Chain ()-- ^ The action to modify the chain state.
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
                  putStrLn $ "  Source: " ++ show (showAddress source)
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
                               show . stakeReference $ head sources
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
createToken :: [TxIn]              -- ^ The UTxOs to spend.
            -> (AddressAny, Value) -- ^ The destination and total value.
            -> Chain ()            -- ^ The action to modify the chain state.
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
        putStrLn $ "  Destination: " ++ showAddress destination
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
        result <- runMantraToIO $ mint context inputs destination value message
        either print return result


-- | Print diagnostic information for a rollback.
printRollback :: (Origins , Origins ) -- ^ The prior and posterior tracking of transaction origins.
              -> (Pendings, Pendings) -- ^ The prior and posterior queues for minting.
              -> Chain ()             -- ^ The action to modify the chain state.
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
printOrigins :: String  -- ^ The prefatory message.
             -> Origins -- ^ The transaction origins.
             -> IO ()   -- ^ The action to print the information.
printOrigins message origins' =
  do
    putStrLn $ "    " ++ message
    sequence_
      [
        do
          putStrLn $ "      " ++ show output
          putStrLn $ "        Source: " ++ show (showAddress source)
      |
        (output, source) <- M.toList origins'
      ]


-- | Print diagnostic information for queued mintings.
printPendings :: String   -- ^ The prefatory message.
              -> Pendings -- ^ The queued mintings.
              -> IO ()    -- ^ The action to print the information.
printPendings message pendings' =
  do
    putStrLn $ "    " ++ message
    sequence_
      [
        do
          putStrLn $ "      " ++ show output
          sequence_
            [
              putStrLn $ "        Source: " ++ show (showAddress source)
            |
              source <- sources
            ]
          printValueIO "        " value
      |
        (output, (sources, value)) <- M.toList pendings'
      ]


-- | Run the chain operations for tracking and minting.
runChain :: MonadFail m
         => MonadIO m
         => Context      -- ^ The service context.
         -> MantraM m () -- ^ Action to run the operations.
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
    watchTransactions
      socket
      protocol
      network
      (Just rollbackHandler)
      idleHandler
      blockHandler
      inHandler
      $ \_ inputs output (TxOut destination txOutValue _) ->
        let
          destination' = toAddressAny' destination
          value =
            case txOutValue of
              TxOutValue   _ value'   -> value'
              TxOutAdaOnly _ lovelace -> lovelaceToValue lovelace
          hasToken = selectAsset value token > 0
          hasImage = not . null $ findPigs scriptHash value
        in
          when (hasToken || hasImage || destination' == keyAddress)
            . withChainState chainState
            $ recordOutput inputs output destination' value


-- | Convert address in an era to any address.
toAddressAny' :: AddressInEra era -- ^ The address in an era.
              -> AddressAny       -- ^ Any address.
toAddressAny' (AddressInEra ByronAddressInAnyEra    address) = toAddressAny address
toAddressAny' (AddressInEra (ShelleyAddressInEra _) address) = toAddressAny address
