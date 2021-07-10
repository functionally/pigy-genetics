module Main (
  main
) where


import Cardano.Api -- (ConsensusModeParams(CardanoModeParams), EpochSlots(..), Network, Protocol, NetworkId(..), NetworkMagic(..), TxOut(..), TxOutValue(..), anyAddressInShelleyBasedEra, deserialiseFromRawBytes, deserialiseFromRawBytesHex, selectAsset, valueToList)
import Mantis.Types (runMantisToIO)
import Pigy.Chain   (pigy)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


main :: IO ()
main =
  do
    let
      socket = "testnet.socket"
      protocol = CardanoModeParams $ EpochSlots 21600
      network = Testnet $ NetworkMagic 1097911063
    result <-
      runMantisToIO
        $ pigy socket protocol network
          "addr_test1qq4mhhat3s522uplguhkk2xxrnwsv65e0yxk9nyrn5530ksvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60sm32w6m"
          "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d"
          "tPIGY"
    case result of
      Right () -> return ()
      Left e   -> hPutStrLn stderr e >> exitFailure
