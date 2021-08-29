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
-- | Types for chain operations.
--
-----------------------------------------------------------------------------


module Pigy.Chain.Types (
-- * Tracking
  Origins
, Pendings
, History
-- * State
, Chain
, ChainState(..)
, activeLens
, currentLens
, originsLens
, pendingsLens
, historyLens
, undosLens
, redosLens
, withChainState
) where


import Cardano.Api                (AddressAny, ScriptHash, SimpleScript(..), SimpleScriptV2, SlotNo(..), TxIn(..), Value)
import Control.Lens               (Lens', lens)
import Control.Monad.State.Strict (StateT(..))
import Data.Default               (Default(..))
import Data.IORef                 (IORef, readIORef, writeIORef)
import Pigy.Types                 (Context(..))

import qualified Data.Map.Strict as M (Map, empty)
import qualified Data.Set        as S (Set, empty)


-- | Map of origins of transactions.
type Origins = M.Map TxIn AddressAny


-- | Map of transactions that to be processed.
type Pendings = M.Map TxIn ([AddressAny], Value)


-- | History of transaction origins and pending transactions.
type History = [(SlotNo, (Origins, Pendings))]


-- | The state of the chain.
data ChainState =
  ChainState
  {
    context       :: Context                     -- ^ The service context.
  , active        :: Bool                        -- ^ Whether the service can mint.
  , current       :: SlotNo                      -- ^ The curent slot number.
  , origins       :: Origins                     -- ^ The originating addresses of UTxOs being tracked.
  , pendings      :: Pendings                    -- ^ Queued minting operations.
  , history       :: History                     -- ^ The transaction history, for rollbacks.
  , undos         :: S.Set TxIn                  -- ^ Transactions that were removed by a rollback.
  , redos         :: S.Set TxIn                  -- ^ Transactions that were re-added by a rollback.
  , scriptAddress :: AddressAny                  -- ^ The minting script address.
  , script        :: SimpleScript SimpleScriptV2 -- ^ The minting script.
  , scriptHash    :: ScriptHash                  -- ^ The hash of the miting script.
  , checker       :: Value -> Bool               -- ^ Function to check validity.
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
    , undos         = S.empty
    , redos         = S.empty
    , scriptAddress = undefined
    , script        = undefined
    , scriptHash    = undefined
    , checker       = undefined
    }


-- | Lens for the active state.
activeLens :: Lens' ChainState Bool
activeLens = lens active $ \x active' -> x {active = active'}


-- | Lens for the current slot number.
currentLens :: Lens' ChainState SlotNo
currentLens = lens current $ \x current' -> x {current = current'}


-- | Lens for the originating addresses being tracked.
originsLens :: Lens' ChainState Origins
originsLens = lens origins $ \x origins' -> x {origins = origins'}


-- | Lens for the queued mintings.
pendingsLens :: Lens' ChainState Pendings
pendingsLens = lens pendings $ \x pendings' -> x {pendings = pendings'}


-- | Lens for the tracking history.
historyLens :: Lens' ChainState History
historyLens = lens history $ \x history' -> x {history = history'}


-- | Lens for the tracking transactions that were removed in a rollback.
undosLens :: Lens' ChainState (S.Set TxIn)
undosLens = lens undos $ \x undos' -> x {undos = undos'}


-- | Lens for the tracking transactions that were re-added in a rollback.
redosLens :: Lens' ChainState (S.Set TxIn)
redosLens = lens redos $ \x redos' -> x {redos = redos'}


-- | The monad for the chain state.
type Chain a = StateT ChainState IO a


-- | Modify the chain state.
withChainState :: IORef ChainState -- ^ Reference to the chain state.
               -> Chain a          -- ^ Action for modifying the chain state.
               -> IO a             -- ^ Action returning the result of the modification.
withChainState ref transition =
  do
    initial <- readIORef ref
    (result, final) <- runStateT transition initial
    writeIORef ref final
    return result
