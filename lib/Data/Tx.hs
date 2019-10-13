module Data.Tx where

import Numeric.Positive
import qualified Numeric.NonNegative.Wrapper as NN
-- I'm using NonNegative rather than GHC.Natural because it can contain an Int.
import Data.Set (Set, empty)

-- | Transaction id, unique to a 'Mempool'.
newtype TxId = TxId NN.Int
  deriving (Eq, Ord, Show)

-- | Number of output within a UTXO. Not globally unique.
type OutputNum = NN.Int

-- | Identifier for a single output of a specific transaction. This id is
-- unique to a 'Mempool'.
data TxoId = TxoId TxId OutputNum
  deriving (Eq, Ord, Show)

-- | Create a unique identifier for a transaction output.
makeTxoId :: Int -> Int -> TxoId
makeTxoId txId n = TxoId (TxId $ NN.fromNumber txId) (NN.fromNumber n)

-- | Transaction output.
data Txo = Txo { amount :: Positive }
  deriving (Eq, Ord, Show)

data Tx = Tx
  { inputs :: Set TxoId
  , outputs :: [Txo]
  }
  deriving (Eq, Ord, Show)

-- | Create a transaction with no inputs, just outputs.
outputsToTx :: [Int] -> Tx
outputsToTx xs = Tx empty outputs
  where
    outputs = intToTxo <$> xs
    intToTxo = Txo . toEnum
