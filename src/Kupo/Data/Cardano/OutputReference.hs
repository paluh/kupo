module Kupo.Data.Cardano.OutputReference where

import Kupo.Prelude

import Kupo.Data.Cardano.Output
    ( Output
    )
import Kupo.Data.Cardano.OutputIndex
    ( OutputIndex
    , outputIndexFromText
    , outputIndexToText
    )
import Kupo.Data.Cardano.TransactionId
    ( HasTransactionId (..)
    , TransactionId
    , transactionIdFromText
    , transactionIdToText
    )
import Kupo.Data.Cardano.TransactionIndex
    ( TransactionIndex
    )

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.Text as T

-- Input

type Input =
    Ledger.TxIn

-- OutputReference

type OutputReference =
    Input

-- ExtendedOutputReference

type ExtendedOutputReference =
    (OutputReference, TransactionIndex)

-- InputReference

-- It happens that we encode inputs using the same structure as outputs
-- the difference is that an index in this case is a transaction
-- input number.
type InputReference =
    Input

type ExtendedInputReference =
    (OutputReference, TransactionIndex)

getOutputIndex :: OutputReference -> OutputIndex
getOutputIndex (Ledger.TxIn _ (Ledger.TxIx ix)) =
    ix
{-# INLINABLE getOutputIndex #-}

getInputIndex :: InputReference -> OutputIndex
getInputIndex = getOutputIndex

mkOutputReference
    :: TransactionId
    -> OutputIndex
    -> OutputReference
mkOutputReference i =
    Ledger.TxIn i . Ledger.TxIx
{-# INLINABLE mkOutputReference #-}

mkInputReference
    :: TransactionId
    -> OutputIndex
    -> InputReference
mkInputReference = mkOutputReference

withReferences
    :: OutputIndex
    -> TransactionId
    -> [Output]
    -> [(OutputReference, Output)]
withReferences startIndex txId = loop startIndex
  where
    loop ix = \case
        [] -> []
        out:rest ->
            let
                results = loop (next ix) rest
             in
                (mkOutputReference txId ix, out) : results

outputReferenceFromText :: Text -> Maybe OutputReference
outputReferenceFromText txt =
    case T.splitOn "@" txt of
        [outputIndex, txId] -> do
            mkOutputReference
                <$> transactionIdFromText txId
                <*> outputIndexFromText outputIndex
        _malformedTextString ->
            Nothing

outputReferenceToText :: OutputReference -> Text
outputReferenceToText outRef =
    outputIndexToText (getOutputIndex outRef)
    <>
    "@"
    <>
    transactionIdToText (getTransactionId outRef)
