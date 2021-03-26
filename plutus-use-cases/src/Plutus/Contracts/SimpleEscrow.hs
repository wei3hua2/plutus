{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}

module Plutus.Contracts.SimpleEscrow
  where

import           Control.Lens             (makeClassyPrisms, review)
import           Control.Monad            (void)
import           Control.Monad.Error.Lens (throwing)
import           Data.Aeson               (FromJSON, ToJSON)
import           GHC.Generics             (Generic)

import           Ledger                   (Datum (..), DatumHash, PubKeyHash, Slot, TxId, TxOutTx (..), ValidatorHash,
                                           interval, pubKeyAddress, scriptOutputsAt, txId, txSignedBy, valuePaidTo)
import qualified Ledger
import           Ledger.Constraints       (TxConstraints)
import qualified Ledger.Constraints       as Constraints
import           Ledger.Contexts          (TxInfo (..), ValidatorCtx (..))
import           Ledger.Interval          (after, before)
import qualified Ledger.Interval          as Interval
import qualified Ledger.Tx                as Tx
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             (Value, geq)

import           Plutus.Contract
import qualified Plutus.Contract.Typed.Tx as Typed
import qualified PlutusTx                 as PlutusTx
import           PlutusTx.Prelude         hiding (Applicative (..), Semigroup (..), check, foldMap)

import           Prelude                  (Semigroup (..), foldMap)
import qualified Prelude                  as Haskell


-- The simple escrow contract facilitiates and exchange of currencies.


-------------------------------------------------
-- Data
-------------------------------------------------

data EscrowParams =
  EscrowParams
    { payee     :: PubKeyHash
    , paying    :: Value
    , expecting :: Value
    , recipient :: PubKeyHash
    --
    , deadline  :: Slot
    -- ^ Slot after which the contract expires.
    }
    deriving stock (Show, Generic)
PlutusTx.unstableMakeIsData ''EscrowParams
PlutusTx.makeLift ''EscrowParams

type EscrowSchema =
    BlockchainActions
        .\/ Endpoint "lock"   ()
        .\/ Endpoint "refund" ()
        .\/ Endpoint "redeem" ()

data Action
  = Redeem | Refund
PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

data RedeemFailReason = DeadlinePassed
    deriving stock (Haskell.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EscrowError =
    RedeemFailed RedeemFailReason
    | RefundFailed
    | EContractError ContractError
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
makeClassyPrisms ''EscrowError

instance AsContractError EscrowError where
    _ContractError = _EContractError

newtype RefundSuccess = RefundSuccess TxId
    deriving newtype (Haskell.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype RedeemSuccess = RedeemSuccess TxId
    deriving (Haskell.Eq, Show)

data Escrow
instance Scripts.ScriptType Escrow where
    type instance RedeemerType Escrow = Action
    type instance DatumType    Escrow = PubKeyHash


-------------------------------------------------
-- Chain-related
-------------------------------------------------

escrowAddress :: EscrowParams -> Ledger.Address
escrowAddress params = Scripts.scriptAddress (escrowInstance params)

escrowInstance :: EscrowParams -> Scripts.ScriptInstance Escrow
escrowInstance params = Scripts.validator @Escrow
    ($$(PlutusTx.compile [|| validate ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||])
      where
        wrap = Scripts.wrapValidator @PubKeyHash @Action

{-# INLINABLE validate #-}
validate :: EscrowParams -> PubKeyHash -> Action -> ValidatorCtx -> Bool
validate params contributor action ValidatorCtx{valCtxTxInfo}
  = case action of
      Redeem ->
        let notLapsed = deadline params `after` txInfoValidRange valCtxTxInfo
            -- TODO: This condition always fails because this particular
            -- transaction isn't paying anything to the Payee; it's a totally
            -- independent one. Probably this is wrong?
            -- paid      = valuePaidTo valCtxTxInfo (payee params) `geq` expecting params
            paid      = True
         in traceIfFalse "paid" paid && traceIfFalse "notLapsed" notLapsed
      Refund ->
        -- TODO: This is problematic at the moment because the awaitSlot only
        -- returns after the deadline has lapsed; so the Refund validation
        -- would fail because it's lapsed? Need to allow a refund after the
        -- deadline has lapsed? Or something else?
        let lapsed = deadline params `before` txInfoValidRange valCtxTxInfo
            signed = valCtxTxInfo `txSignedBy` contributor
        in traceIfFalse "lapsed" lapsed && traceIfFalse "signed" signed


-------------------------------------------------
-- Endpoints
-------------------------------------------------

-- |
lockEp :: EscrowParams -> Contract () EscrowSchema EscrowError ()
lockEp params = do
  endpoint @"lock"

  pk <- ownPubKey

  logInfo $ "Locking value: " <> show (paying params) <> " for " <> show pk

  let valRange = Interval.to (pred $ deadline params)
      tx = Constraints.mustPayToTheScript (Ledger.pubKeyHash pk) (paying params)
            <> Constraints.mustValidateIn valRange

  t <- submitTxConstraints (escrowInstance params) tx

  logInfo $ "Submitted lock transaction " <> show t


-- |
refundEp :: EscrowParams -> Contract () EscrowSchema EscrowError RefundSuccess
refundEp params = mapError (review _EscrowError) $ endpoint @"refund" >> refund params


-- |
refund :: EscrowParams
       -> Contract () EscrowSchema EscrowError RefundSuccess
refund params = do
  pk <- ownPubKey
  unspentOutputs <- utxoAt (escrowAddress params)

  let totalValue = foldMap (Tx.txOutValue . Tx.txOutTxOut) unspentOutputs

  logInfo $ "Running a refund for " <> show pk <> " of value " <> show totalValue

  let valRange = Interval.from (succ $ deadline params)
      tx = Typed.collectFromScript unspentOutputs Refund
              <> Constraints.mustValidateIn valRange

  if Constraints.modifiesUtxoSet tx
  then RefundSuccess . txId <$> submitTxConstraintsSpending (escrowInstance params) unspentOutputs tx
  else throwing _RefundFailed ()



-- |
redeemEp :: EscrowParams -> Contract () EscrowSchema EscrowError RedeemSuccess
redeemEp params = mapError (review _EscrowError) $ endpoint @"redeem" >> redeem params


-- |
redeem :: EscrowParams
       -> Contract () EscrowSchema EscrowError RedeemSuccess
redeem params = do
  current <- currentSlot
  unspentOutputs <- utxoAt (escrowAddress params)

  let value = foldMap (Tx.txOutValue . Tx.txOutTxOut) unspentOutputs

  logInfo $ "Running a redeem for " <> show (recipient params) <> " of value " <> show value

  let valRange = Interval.to (pred $ deadline params)
      tx = Typed.collectFromScript unspentOutputs Redeem
              <> Constraints.mustPayToPubKey (recipient params) value
              <> Constraints.mustValidateIn valRange

  if current >= deadline params
  then throwing _RedeemFailed DeadlinePassed
  else RedeemSuccess . txId <$> do
    t <- submitTxConstraintsSpending (escrowInstance params) unspentOutputs tx
    logInfo $ "Submitted transaction " <> show t
    return t


-------------------------------------------------
-- Main
-------------------------------------------------

contract :: EscrowParams
         -> Contract () EscrowSchema EscrowError (Either RefundSuccess RedeemSuccess)
contract params
  = outcome >>= \x -> case x of
                        Left _  -> Left  <$> refundEp params
                        Right _ -> Right <$> redeemEp params
    where
      address        = Ledger.PubKeyAddress $ payee params
      deadlineLapsed = awaitSlot (deadline params) >> (logInfo $ ("Slot waiting completed!" :: Haskell.String))
      valuePaidIn    = void $ do
        logInfo $ "Checking value at " <> show address <> " to be at least " <> show (expecting params)
        a <- fundsAtAddressGeq address (expecting params)
        let s = foldMap (Tx.txOutValue . Tx.txOutTxOut) a
        logInfo $ "Found total " <> show s
        return a
      --
      -- TODO: Use `both`, or something, so that someone can call the `refund`
      -- endpoint at any time.
      outcome = lockEp params >> selectEither deadlineLapsed valuePaidIn
