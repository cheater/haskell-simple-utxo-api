# haskell-simple-utxo-api

A simple library defining a minimal Mempool/UTXO node. This is meant as a demo
of this sort of program, and is therefore kept very simple.

# What is this project?

This library defines a very simplified version of payment transactions as known
from Bitcoin.

## What this library allows

1. Define transactions with inputs and outputs

1. Submit transactions to the mempool.

1. It will keep track of transactions in the mempool, as well as the current
   set of UTXOs.

1. It will perform basic validity checks on the transactions, such as data
   integrity checks, input validation, and overspending protection.

1. You can create transactions that create money, by bypassing the overspending
   protection.

## The missing parts

1. Error reporting is very simple. It will report if a transaction is invalid,
   and it's up to the user to figure out why.

1. No binary representation. Transactions as well as related data are all basic
   Haskell types.

1. Transactions are identified by sequential id, not by hash.

1. No signing. There is no cryptographic proof of access. In Bitcoin, money is
   stored inside outputs, the contents of which cannot be spent, unless you
   know a private key that allows you access to the output in question. Every
   output can have a different private key used to unlock it. In this code, it
   is not possible to define a key, therefore all outputs belong to everyone.

1. No mining. You can, however, define transactions that have more money in the
   outputs than in the inputs.

1. No peer-to-peer networking. This library is a simplified mempool, but it
   does not allow you to perform an action equivalent to mining a block and
   sending it out to the blockchain.

1. No blockchain. Only mempool.

# Usage

Launch GHCi and import the module, using `cabal repl` or `stack repl`.

```Haskell
import Data.Mempool
import Data.Tx
```

Create a new Mempool and UTXOs:

```Haskell
let mempool = createMempool
let utxos = createUTXOs
```

Define a new transaction:
```Haskell
let outputs = [17, 10, 99, 3, 1500]
let tip = outputsToTx outputs
```

Add the transaction to the existing Mempool and UTXOs. We'll skip overspending
checks because the mempool doesn't currently have any money on it to spend, and
we need to add some.
```Haskell
let checkOverspend = False
let Right (mempool', utxos') = addTxs mempool utxos checkOverspend [tip]

print mempool'
print utxos'
```

Add another transaction, which spends existing money and does not create any
money of its own:

```Haskell
let inputs = head . toList $ utxos'
let outputs = [Txo 1, Txo 2]
let tx = Tx inputs outputs

let checkOverspend = True
let Right (mempool'', utxos'') = addTxs mempool utxos checkOverspend [tx]
```

# Building

You may use `stack` or `cabal` to build this package.

# Testing

You may use `cabal test --enable-tests --test-show-details=direct` or
`stack test` to run the tests, depending on whether you prefer `cabal` or
`stack`. The tests are written in a (hopefully) easy to understand way, and
they should be a good source of material for someone who wants to learn the
basics of a Mempool and UTXOs.
