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
-- * Addresses
  MaryAddress
, MaryScript
-- * Tracking
, Origins
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
, withChainState
) where


import Cardano.Api                (AddressInEra(..), MaryEra, ScriptHash, ScriptInEra(..), SlotNo(..), TxIn(..), Value)
import Control.Lens               (Lens', lens)
import Control.Monad.State.Strict (StateT(..))
import Data.Default               (Default(..))
import Data.IORef                 (IORef, readIORef, writeIORef)
import Pigy.Types                 (Context(..))

import qualified Data.Map.Strict as M  (Map, empty)


-- | A Mary address.
type MaryAddress = AddressInEra MaryEra


-- | A Mary script.
type MaryScript = ScriptInEra MaryEra


-- | Map of origins of transactions.
type Origins = M.Map TxIn MaryAddress


-- | Map of transactions that to be processed.
type Pendings = M.Map TxIn ([MaryAddress], Value)


-- | History of transaction origins and pending transactions.
type History = [(SlotNo, (Origins, Pendings))]


-- | The state of the chain.
data ChainState =
  ChainState
  {
    context       :: Context        -- ^ The service context.
  , active        :: Bool           -- ^ Whether the service can mint.
  , current       :: SlotNo         -- ^ The curent slot number.
  , origins       :: Origins        -- ^ The originating addresses of UTxOs being tracked.
  , pendings      :: Pendings       -- ^ Queued minting operations.
  , history       :: History        -- ^ The transaction history, for rollbacks.
  , scriptAddress :: MaryAddress    -- ^ The minting script address.
  , script        :: MaryScript     -- ^ The minting script.
  , scriptHash    :: ScriptHash     -- ^ The hash of the miting script.
  , checker       :: Value -> Bool  -- ^ Function to check validity.
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
