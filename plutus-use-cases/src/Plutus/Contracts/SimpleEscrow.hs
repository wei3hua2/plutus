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
import           PlutusTx.Prelude         hiding (Applicative (..), Semigroup (..), check)

import           Prelude                  (Semigroup (..))
import qualified Prelude                  as Haskell


-- The simple escrow contract facilitiates and exchange
-- of currencies.
--
-- A contract is created with the deal:
--
--  The recipient will receive X value
--    from the payee, who will receive in turn Y value
--
--  i.e. Alice and Bob perform an exchange of 10 Ada for 9 Bitcoin.


-------------------------------------------------
-- Data
-------------------------------------------------

data EscrowParams =
  EscrowParams
    { party1   :: PubKeyHash
    -- ^ The person who must receive the transfer.
    , redeemer :: PubKeyHash
    -- ^ The person who can cash in on the output of this contract.
    , value    :: Value
    -- ^ The value that they need to receive.
    , deadline :: Slot
    -- ^ Slot after which the contract expires.
    }
    deriving stock (Show, Generic)
PlutusTx.unstableMakeIsData ''EscrowParams
PlutusTx.makeLift ''EscrowParams

type EscrowSchema =
    BlockchainActions
        .\/ Endpoint "lock"   Value
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
        let notLapsed = (deadline params) `after` txInfoValidRange valCtxTxInfo
            paid      = valuePaidTo valCtxTxInfo (party1 params) `geq` (value params)
         in traceIfFalse "paid" paid && traceIfFalse "notLapsed" notLapsed
      Refund ->
        let lapsed = (deadline params) `before` txInfoValidRange valCtxTxInfo
            signed = valCtxTxInfo `txSignedBy` contributor
        in traceIfFalse "lapsed" lapsed && traceIfFalse "signed" signed


-------------------------------------------------
-- Endpoints
-------------------------------------------------

-- |
lockEp :: EscrowParams -> Contract () EscrowSchema EscrowError ()
lockEp params = do
  v  <- endpoint @"lock"
  pk <- ownPubKey

  logInfo $ "Locking value: " <> show v <> " for " <> show pk

  let valRange = Interval.to (pred $ deadline params)
      tx = Constraints.mustPayToTheScript (Ledger.pubKeyHash pk) v
            <> Constraints.mustValidateIn valRange

  void $ submitTxConstraints (escrowInstance params) tx


-- |
refundEp :: EscrowParams -> Contract () EscrowSchema EscrowError RefundSuccess
refundEp params = mapError (review _EscrowError) $ endpoint @"refund" >> refund params


-- |
refund :: EscrowParams
       -> Contract () EscrowSchema EscrowError RefundSuccess
refund params = do
  pk <- ownPubKey
  unspentOutputs <- utxoAt (escrowAddress params)

  logInfo $ "Running a refund for " <> show pk <> " of " <> show unspentOutputs

  let valRange = Interval.from (succ $ deadline params)
      tx = Typed.collectFromScript unspentOutputs Refund
              <> Constraints.mustValidateIn valRange

  if Constraints.modifiesUtxoSet tx
  then RefundSuccess . txId <$> submitTxConstraintsSpending (escrowInstance params) unspentOutputs tx
  else throwing _RefundFailed ()


-- |
redeem :: EscrowParams
       -> Contract () EscrowSchema EscrowError RedeemSuccess
redeem params = do
  current <- currentSlot
  unspentOutputs <- utxoAt (escrowAddress params)

  logInfo $ "Running a redeem for " <> show (redeemer params) <> " of " <> show unspentOutputs <> " at " <> show current

  let valRange = Interval.to (pred $ deadline params)
      tx = Typed.collectFromScript unspentOutputs Redeem
              <> Constraints.mustPayToPubKey (redeemer params) (value params)
              <> Constraints.mustValidateIn valRange

  if current >= deadline params
  then throwing _RedeemFailed DeadlinePassed
  else RedeemSuccess . txId <$> submitTxConstraintsSpending (escrowInstance params) unspentOutputs tx


-------------------------------------------------
-- Main
-------------------------------------------------

contract :: EscrowParams
         -> Contract () EscrowSchema EscrowError (Either RefundSuccess RedeemSuccess)
contract params
  = outcome >>= \x -> case x of
                        Left _  -> Left  <$> refund params
                        Right _ -> Right <$> redeem params
    where
      address        = Ledger.PubKeyAddress $ party1 params
      deadlineLapsed = awaitSlot (deadline params)
      valuePaidIn    = void $ do
        logInfo $ "Checking value at " <> show address <> " to be at least " <> show (value params)
        fundsAtAddressGeq address (value params)
      --
      -- TODO: Use `both`, or something, so that we someone can call the
      -- `refund` endpoint.
      outcome = lockEp params >> selectEither deadlineLapsed valuePaidIn
