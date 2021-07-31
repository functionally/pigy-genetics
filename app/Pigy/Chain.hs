
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}


module Pigy.Chain (
  runChain
) where


import Cardano.Api                                       (AddressInEra(..), AssetId(..), AssetName(..), BlockHeader(..), ChainPoint, MaryEra, PolicyId(..), Quantity(..), ScriptHash, ScriptInEra(..), ShelleyWitnessSigningKey(..), SlotNo(..), StakeAddressReference(NoStakeAddress), TxIn(..), TxMetadata(..), TxMetadataValue(..), TxOut(..), TxOutValue(..), Value, filterValue, getTxId, makeScriptWitness, makeShelleyKeyWitness, makeSignedTransaction, makeTransactionBody, negateValue, quantityToLovelace, selectAsset, selectLovelace, serialiseToRawBytesHexText, valueFromList, valueToList)
import Control.Monad                                     (unless, when)
import Control.Monad.Error.Class                         (throwError)
import Control.Monad.IO.Class                            (MonadIO, liftIO)
import Control.Monad.State.Strict                        (MonadState(..), StateT(..), modify)
import Data.Default                                      (Default(..))
import Data.IORef                                        (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe                                        (mapMaybe)
import Mantis.Chain                                      (watchTransactions)
import Mantis.Query                                      (submitTransaction)
import Mantis.Script                                     (mintingScript)
import Mantis.Types                                      (MantisM, foistMantisEither, printMantis, runMantisToIO)
import Mantis.Transaction                                (includeFee, makeTransaction, printValueIO, supportedMultiAsset)
import Mantis.Wallet                                     (showAddressMary, stakeReferenceMary)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import Pigy.Image                                        (crossover, fromChromosome, newGenotype)
import Pigy.Ipfs                                         (pinImage)
import Pigy.Types                                        (Context(..), KeyedAddress(..))

import qualified Data.ByteString.Char8 as BS  (ByteString, drop, isPrefixOf, pack, unpack)
import qualified Data.Map.Strict       as M   (Map, delete, empty, fromList, fromListWith, insert, lookup, member, null, toList)
import qualified Data.Text             as T   (pack)


type MaryAddress = AddressInEra MaryEra


type MaryScript = ScriptInEra MaryEra


-- | Map of origins of transactions.
type Origins = M.Map TxIn MaryAddress


-- | Map of transactions that to be processed.
type Pendings = M.Map TxIn ([MaryAddress], Value)


-- | History of transaction origins and pending transactions.
type History = [(SlotNo, (Origins, Pendings))]


-- | The Ouroboros security parameter.
kSecurity :: Int
kSecurity = 2160


record :: SlotNo
       -> (Origins, Pendings)
       -> History
       -> History
record slot sourcePending = take kSecurity . ((slot, sourcePending) :)


rollback :: SlotNo
         -> History
         -> History
rollback slot = dropWhile $ (/= slot) . fst


toSlotNo :: ChainPoint
         -> SlotNo
toSlotNo point =
  -- FIXME: Use safe way to find the slot number at a chain point.
  case show point of
    "ChainPointAtGenesis" -> SlotNo 0
    text                  -> SlotNo . read . takeWhile (/= ')') $ drop 19 text


data ChainState =
  ChainState
  {
    context       :: Context
  , active        :: Bool
  , current       :: SlotNo
  , origins       :: Origins
  , pendings      :: Pendings
  , history       :: History
  , scriptAddress :: MaryAddress
  , script        :: MaryScript
  , scriptHash    :: ScriptHash
  , checker       :: Value -> Bool
  }

instance Default ChainState where
  def =
    ChainState
    {
      context       = undefined
    , active        = False
    , current       = SlotNo 0
    , origins       = M.empty
    , pendings      = M.empty
    , history       = [(SlotNo 0, (M.empty, M.empty))]
    , scriptAddress = undefined
    , script        = undefined
    , scriptHash    = undefined
    , checker       = undefined
    }


type Chain a = StateT ChainState IO a


withChainState :: IORef ChainState
               -> Chain a
               -> IO a
withChainState ref transition =
  do
    initial <- readIORef ref
    (result, final) <- runStateT transition initial
    writeIORef ref final
    return result


makeActive :: Chain ()
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
          $ \x -> x {active = True}


recordBlock :: SlotNo
            -> Chain ()
recordBlock slot =
  do
    ChainState{..} <- get
    when (verbose context && active)
      . liftIO
      $ do
        putStrLn ""
        putStrLn $ "New block: " ++ show current ++ " -> " ++ show slot
    modify
      $ \x ->
        x {
            current = slot
          , history = record current (origins, pendings) history
          }


recordRollback :: SlotNo
               -> Chain ()
recordRollback slot =
  do
    ChainState{..} <- get
    liftIO
      $ do
        putStrLn ""
        putStrLn $ "Rollback: " ++ show slot ++ " <- " ++ show current
    printPending "    Prior pendings:"
    let
      history'@((_, (origins', pendings')) : _) = rollback slot history
    modify
      $ \x -> x {
                  current  = slot
                , origins  = origins'
                , pendings = pendings'
                , history  = history'
                }
    printPending "    Posterior pendings:"


recordInput :: SlotNo
            -> TxIn
            -> Chain ()
recordInput slot txIn =
  do
    ChainState{..} <- get
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
        state
          $ \x ->
            (
              ()
            , x {
                  origins  = txIn `M.delete` origins
                , pendings = txIn `M.delete` pendings
                }
            )


recordOutput :: [TxIn]
             -> TxIn
             -> MaryAddress
             -> Value
             -> Chain ()
recordOutput inputs output destination value =
  do
    ChainState{..} <- get
    modify
      $ \x -> x {origins = M.insert output destination origins}
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
            putStrLn $ "  Source: " ++ show (showAddressMary source')
          |
            source' <- sources
          ]
        putStrLn $ "  Destination: " ++ showAddressMary destination
        putStrLn $ "  Stake: " ++ show (stakeReferenceMary destination)
        putStrLn $ "  To me: " ++ show (destination == scriptAddress)
        putStrLn $ "  Valid: " ++ show valid
        printValueIO "  " value
    when (destination == scriptAddress && not (null sources))
      $ if active && valid
          then createToken [output] (head sources, value)
          else do
                 liftIO $ putStrLn "  Queued for creation."
                 modify $ \x -> x {pendings = M.insert output (sources, value) pendings}


createPendingSingle :: Chain ()
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


createPendingMultiple :: Chain ()
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
              putStrLn "Multiple input transactions:"
              putStrLn $ "  Stake: " ++ stake
              sequence_
                [
                  putStrLn $ "  Source: " ++ show (showAddressMary source)
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
                               show . stakeReferenceMary $ head sources
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


createToken :: [TxIn]
            -> (MaryAddress, Value)
            -> Chain ()
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
        putStrLn $ "  Destination: " ++ showAddressMary destination
        putStrLn $ "  To me: "++ show (destination == scriptAddress)
        printValueIO "  " value
    sequence_
      [
        -- FIXME: Consider deleting the transactions only if the minting succeeeds.
        modify
          $ \x -> x {
                      origins  = input `M.delete` origins
                    , pendings = input `M.delete` pendings
                    }
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
        case result of
          Right () -> return ()
          Left  e  -> putStrLn $ "  " ++ e


printPending :: String
             -> Chain ()
printPending message =
  do
    ChainState{..} <- get
    unless (M.null pendings)
      .  liftIO
      $ do
        putStrLn message
        sequence_
          [
            do
              putStrLn $ "    " ++ show output
              sequence_
                [
                  putStrLn $ "      Source: " ++ show (showAddressMary source)
                |
                  source <- sources
                ]
              printValueIO "      " value
          |
            (output, (sources, value)) <- M.toList pendings
          ]


runChain :: MonadFail m
         => MonadIO m
         => Context
         -> MantisM m ()
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
            createPendingSingle
            createPendingMultiple
            return False
      inHandler (BlockHeader slot _ _) txIn =
        withChainState chainState
          $ recordInput slot txIn
      outHandler _ inputs output (TxOut destination txOutValue) =
        case txOutValue of
          TxOutValue _ value ->
            when (selectAsset value token > 0 || destination == keyAddress)
              . withChainState chainState
              $ recordOutput inputs output destination value
          _ -> return ()
    watchTransactions
      socket
      protocol
      network
      (Just rollbackHandler)
      idleHandler
      blockHandler
      inHandler
      outHandler


checkValue :: AssetId
           -> ScriptHash
           -> Value
           -> Bool
checkValue token scriptHash value =
  let
    ada = selectLovelace value
    pigy = selectAsset value token
    pigs = maximum[1, length $ findPigs scriptHash value]
    a = 1_500_000
    b =   500_000
  in
    pigy > 0 && ada >= quantityToLovelace (Quantity $ a + b * fromIntegral pigs)


pigFilter :: ScriptHash
          -> AssetId
          -> Bool
pigFilter scriptHash (AssetId (PolicyId scriptHash') (AssetName name')) = scriptHash' == scriptHash && BS.isPrefixOf "PIG@" name'
pigFilter _ _ = False


findPigs :: ScriptHash
         -> Value
         -> [BS.ByteString]
findPigs scriptHash =
  map (\(AssetId _ (AssetName name'), _) -> BS.drop 4 name')
    . filter (pigFilter scriptHash . fst)
    . valueToList


mint :: MonadFail m
     => MonadIO m
     => Context
     -> [TxIn]
     -> MaryAddress
     -> Value
     -> [String]
     -> MantisM m ()
mint Context{..} txIns destination value message =
  do
    let
      KeyedAddress{..} = keyedAddress
      (script, scriptHash) = mintingScript verificationHash Nothing
      pigs = findPigs scriptHash value
    (metadata, minting) <-
      if length pigs == 1
        then do
               printMantis $ "  Burnt token: " ++ BS.unpack (head pigs)
               return
                 (
                   Nothing
                 , negateValue $ filterValue (pigFilter scriptHash) value
                 )
        else do
               genotype <-
                 liftIO
                   $ if null pigs
                       then do
                              putStrLn "  New token."
                              newGenotype gRandom
                       else do
                              putStrLn $ "  Crossover token: " ++ show (BS.unpack <$> pigs)
                              crossover gRandom $ mapMaybe (fromChromosome . BS.unpack) pigs
               (chromosome, cid) <- pinImage ipfsPin images genotype
               let
                 name = "PIG@" ++ chromosome
               return
                 (
                   Just
                     . TxMetadata
                     $ M.fromList
                     [
                       (
                         721
                       , TxMetaMap
                         [
                           (
                             TxMetaText . serialiseToRawBytesHexText $ scriptHash
                           , TxMetaMap
                             [
                               (
                                 TxMetaText $ T.pack name
                               , TxMetaMap
                                 [
                                   (TxMetaText "name"      , TxMetaText . T.pack $ "PIG " ++ chromosome                         )
                                 , (TxMetaText "image"     , TxMetaText $ "ipfs://" <> T.pack cid                               )
                                 , (TxMetaText "ticker"    , TxMetaText $ T.pack name                                           )
                                 , (TxMetaText "parents"   , TxMetaList $ TxMetaText . T.pack . ("PIG@" ++) . BS.unpack <$> pigs)
                                 , (TxMetaText "url"       , TxMetaText "https://pigy.functionally.live"                        )
                                 ]
                               )
                             ]
                           )
                         ]
                       )
                     , (
                         674
                       , TxMetaMap
                         [
                           (
                             TxMetaText "msg"
                           , TxMetaList $ TxMetaText . T.pack <$> message
                           )
                         ]
                       )
                     ]
                 , valueFromList [(AssetId (PolicyId scriptHash) (AssetName $ BS.pack name),  1)]
                 )
    let
      value' = value <> minting
    txBody <- includeFee network pparams 1 1 1 0
      $ makeTransaction
        txIns
        [TxOut destination (TxOutValue supportedMultiAsset value')]
        Nothing
        metadata
        Nothing
        (Just minting)
    txRaw <- foistMantisEither $ makeTransactionBody txBody
    let
      witness = makeShelleyKeyWitness txRaw
        $ WitnessPaymentExtendedKey signing
      witness' = makeScriptWitness script
      txSigned = makeSignedTransaction [witness, witness'] txRaw
    result <- submitTransaction socket protocol network txSigned
    case result of
      SubmitSuccess     -> printMantis $ "  Success: " ++ show (getTxId txRaw)
      SubmitFail reason -> throwError  $ "  Failure: " ++ show reason
