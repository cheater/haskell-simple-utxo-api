module Data.Mempool where

import Data.Tx
import Data.Set (Set, member, union, fromList, toList, (\\))
import qualified Numeric.NonNegative.Wrapper as NN
import Data.Foldable (foldl')

-- | Collect transactions that have been committed by the clients.
type Mempool = [Tx]

-- | Start a mempool.
createMempool :: Mempool
createMempool = mempty

-- | Collect 'TxoId's of transaction outputs that have not been spent.
type UTXOs = Set TxoId

-- | Start a UTXO pool.
createUTXOs :: UTXOs
createUTXOs = mempty

-- | Ensure that a transaction is valid based on the given 'Mempool' and
-- 'UTXOs' (debug version).
txValid' :: Mempool -> UTXOs -> Bool -> Tx -> Bool
txValid' mempool utxos checkOverspend tx@(Tx txInputs _) =
  all (txInputValid mempool utxos) txInputs
  && ((not checkOverspend) || txNoOverspend mempool tx)

-- | Ensure that a transaction is valid based on the given 'Mempool' and
-- 'UTXOs'.
txValid :: Mempool -> UTXOs -> Tx -> Bool
txValid mempool utxos tx =
  txValid' mempool utxos True tx

-- | Ensure that the inputs to a transaction are enough for what it spends.
txNoOverspend :: Mempool -> Tx -> Bool
txNoOverspend mempool (Tx txInputs txOutputs) = ins >= outs
  where
    -- we need to cast to Integer, otherwise sum over an empty list will try to
    -- create a Positive of value 0, resulting in a runtime exception.
    ins :: Integer
    ins = sum $ toInteger . amount . (getTxo mempool) <$> toList txInputs
    outs :: Integer
    outs = sum $ toInteger . amount <$> txOutputs

-- | Ensure that inputs to the transaction exist and there's no double
-- spending.
txInputValid :: Mempool -> UTXOs -> TxoId -> Bool
txInputValid mempool utxos txoId =
  txoIdExists mempool txoId && txoId `member` utxos

-- | Ensure that a transaction output with a specific 'TxoId' exists in the
-- 'Mempool'.
txoIdExists :: Mempool -> TxoId -> Bool
txoIdExists mempool (TxoId txId oid) =
  length (outputs $ getTx mempool txId) >= (NN.toNumber oid)

-- | Get a transaction from the 'Mempool' by its id.
getTx :: Mempool -> TxId -> Tx
getTx mempool (TxId txId) = mempool !! (NN.toNumber txId)

-- | Get a transaction output from the 'Mempool' by its id.
getTxo :: Mempool -> TxoId -> Txo
getTxo mempool (TxoId (TxId txId) outputNum) = output
  where
    tx = mempool !! NN.toNumber txId
    output = outputs tx !! NN.toNumber outputNum

-- | Explains why a transaction is invalid.
newtype InvalidTx = InvalidTx String
-- NOTE: I would prefer an ADT instead of String.
  deriving Show

-- | Replace a list with sequential indices starting with 0.
indices0 :: [a] -> [Int]
indices0 = zipWith const [0..]

-- | Add a transaction to the mempool and mark spent transaction outputs.
--
-- NOTE: for better performance, marking spent transaction outputs could happen
-- at the same time as validating that the outputs do exist, which could all
-- happen in a single scan. This is currently not necessary.
--
-- Data structures that allow appending, unlike Haskell's list for the Mempool
-- type, and a better data structure than Set for the UTXOs type, would make
-- this code perform better.
addTx :: Mempool -> UTXOs -> Bool -> Tx -> Either InvalidTx (Mempool, UTXOs)
addTx mempool utxos checkOverspend tx =
  if txValid' mempool utxos checkOverspend tx
    then
      let newMempool = mempool <> [tx]

          newUtxos :: Set TxoId
          newUtxos = (utxos \\ spent) `union` newOutputs

          spent :: Set TxoId
          spent = inputs tx

          newOutputs :: Set TxoId
          newOutputs = fromList $ (makeTxoId txId) <$> (indices0 $ outputs tx)

          txId = length newMempool - 1


      in Right (newMempool, newUtxos)
    else
      Left . InvalidTx $ "Invalid transaction."

-- | Add multiple transactions to a 'Mempool' or abort all transactions on
-- encountering the first invalid transaction, and report the error.
addTxs :: Mempool -> UTXOs -> Bool -> [Tx] -> Either InvalidTx (Mempool, UTXOs)
addTxs mempool utxos checkOverspend txs = foldl' add acc txs
  where
    acc = Right (mempool, utxos)
    add (Left err) _ = Left err
    add (Right (mempool, utxos)) tx = addTx mempool utxos checkOverspend tx
