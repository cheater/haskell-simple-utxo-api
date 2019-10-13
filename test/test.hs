{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Mempool
import Data.Tx

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (empty)
import Data.Foldable (foldrM)
import Data.Set (fromList, toList, intersection, union)
import qualified Data.Set as S

-- | Hedgehog generator that yields outputs for a single transaction.
genOutputs :: Gen [Int]
genOutputs = Gen.list (Range.linear 1 20) (Gen.enum 1 1000)

-- | Hedgehog generator that yields outputs for multiple transactions.
genTxosRaw :: Gen [[Int]]
genTxosRaw = Gen.list (Range.linear 1 10) genOutputs

-- | Hedgehog generator that yields a list of transactions.
genTxos :: Gen [Tx]
genTxos = (outputsToTx <$>) <$> genTxosRaw -- one <$> to get inside Gen,
                                           -- another to get inside [].

-- | Use a list of truth values to select from a list of elements.
chooseWith :: [a] -> [Bool] -> [a]
chooseWith xs bs = map snd . filter fst $ zip bs xs

-- | Generate a list of truth values of a certain length.
genBoolsN :: Int -> Gen [Bool]
genBoolsN n = Gen.list (Range.constant n n) Gen.bool

-- | Set up a 'Mempool' and 'UTXOs' with a list of transactions, all of which
-- have no inputs, just outputs.
setupTxs :: Monad m => PropertyT m ([Tx], [TxoId])
setupTxs = do
    (txosRaw :: [[Int]]) <- forAll genTxosRaw
    let txs :: [Tx]
        txs = outputsToTx <$> txosRaw

    let txIds :: [Int]
        txIds = indices0 txosRaw
    let utxoIdsL :: [[TxoId]]
        utxoIdsL = flip map txIds $ \txId ->
          let
            txOutputs :: [Int]
            txOutputs = txosRaw !! txId

            outputs :: [Int]
            outputs = indices0 txOutputs

            txoIds :: [TxoId]
            txoIds = (makeTxoId txId) <$> outputs
          in txoIds

    let utxoIds :: [TxoId]
        utxoIds = concat utxoIdsL

    return (txs, utxoIds)

-- | Set up a new 'Mempool' and 'UTXOs'.
setupMempool :: (Mempool, UTXOs)
setupMempool = (mempool, utxos)
  where
    mempool :: [Tx]
    mempool = createMempool

    utxos = createUTXOs

-- | Create 'Mempool' and 'UTXOs' from a list of transactions, and run an
-- action on either failure or success.
withMempoolDef txs failure success =
  withMempool mempool utxos txs failure success
  where (mempool, utxos) = setupMempool

-- | Add a list of transactions to provided 'Mempool' and 'UTXOs', and run an
-- action on either failure or success. Lets you define whether overspending
-- should be checked for, in order to allow test set-up.
withMempool' mempool utxos checkOverspend txs failure success =
  let eitherMU = addTxs mempool utxos checkOverspend txs
  in case eitherMU of
       Left _ -> failure
       Right (mempool, utxos) -> success mempool utxos

-- | Add a list of transactions to provided 'Mempool' and 'UTXOs', and run an
-- action on either failure or success. Don't perform checks for overspending.
withMempool mempool utxos txs failure success =
  withMempool' mempool utxos False txs failure success

-- | Ensure that spent UTXOs are remvoed from 'UTXOs'.
prop_spentRemoved :: Property
prop_spentRemoved =
  property $ do
    (txs, _) <- setupTxs
    let retry = empty -- retry the test, test data was generated wrong
    withMempoolDef txs retry $ \mempool utxos -> do
      -- add extra transaction to the mempool, check if its inputs were removed
      -- from utxos
      chosenInputs <- forAll . genBoolsN $ length utxos
      let inputs = fromList $ chooseWith (toList utxos) chosenInputs
          outputs = []
          tx = Tx inputs outputs
      withMempool mempool utxos [tx] retry $ \_ utxosNew -> do
        inputs `intersection` utxosNew === S.empty
        inputs `union` utxosNew === utxos

-- | Ensure that transactions cannot spend more in their outputs than they take
-- in using their inputs.
prop_noOverspend :: Property
prop_noOverspend =
  property $ do
    (txs, _) <- setupTxs
    let retry = empty -- retry the test, test data was generated wrong
    withMempoolDef txs retry $ \mempool utxos -> do
      -- add extra transaction to the mempool, check if its inputs were removed
      -- from utxos
      chosenInputs <- forAll . genBoolsN $ length utxos
      overspend <- forAll $ Gen.integral (Range.linear 1 1000)
      let inputs = fromList $ chooseWith (toList utxos) chosenInputs
          inputAmount =
            sum $ toInteger . amount . (getTxo mempool) <$> toList inputs
          outputs = [Txo . fromInteger $ inputAmount + overspend]
          tx = Tx inputs outputs
      withMempool' mempool utxos True [tx] success $ \_ _ -> failure

-- Useful function for printing out debug strings.
-- dbg s x = liftIO . putStrLn $ s ++ ": " ++ (take 4000 $ show x)

-- | Ensure that transaction outputs get added to 'UTXOs'.
prop_addedToUtxos :: Property
prop_addedToUtxos =
  property $ do
    (txs, utxoIds) <- setupTxs
    let retry = empty -- retry the test, test data was generated wrong
    withMempoolDef txs retry $ \_ utxos ->
      fromList utxoIds === utxos

-- | Ensure that valid transactions get added to the 'Mempool'.
prop_addedToMempool :: Property
prop_addedToMempool =
  property $ do
    (txs, utxoIds) <- setupTxs
    let retry = empty -- retry the test, test data was generated wrong
    withMempoolDef txs retry $ \mempool _ ->
      fromList mempool === fromList txs

main = do
  check prop_addedToUtxos
  check prop_addedToMempool
  check prop_spentRemoved
  check prop_noOverspend
