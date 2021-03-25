{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module Spec.SimpleEscrow(tests) where

import           Control.Monad                 (void)

import           Ledger                        (pubKeyHash)
import qualified Ledger.Ada                    as Ada
import           Plutus.Contract
import           Plutus.Contract.Test

import           Plutus.Contracts.SimpleEscrow (EscrowParams (..), contract, lockEp, refundEp)

import qualified Plutus.Trace.Emulator         as Trace

import           Test.Tasty

tests :: TestTree
tests = testGroup "simple-escrow"
    [ checkPredicate "can lock some value in the contract"
        ( walletFundsChange w1 (Ada.lovelaceValueOf (-10))
        )
        $ do
          let c = void $ lockEp escrowParams
          hdl <- Trace.activateContractWallet w1 c
          Trace.callEndpoint @"lock" hdl (Ada.lovelaceValueOf 10)
          void $ Trace.waitNSlots 1


    , checkPredicate "can lock and refund"
        ( walletFundsChange w1 mempty
        )
        $ do
          let c = void (both (lockEp escrowParams) (refundEp escrowParams))

          hdl <- Trace.activateContractWallet w1 c
          Trace.callEndpoint @"lock" hdl (Ada.lovelaceValueOf 10)
          void $ Trace.waitNSlots 1

          Trace.callEndpoint @"refund" hdl ()
          void $ Trace.waitNSlots 1


    -- , let c = void $ contract escrowParams in
    --   checkPredicate "can redeem if party is paid their due"
    --     ( assertDone c (Trace.walletInstanceTag w1) (const True) "escrow can redeem not done"
    --     .&&. walletFundsChange w1 (Ada.lovelaceValueOf 20)
    --     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 10)
    --     )
    --     $ do
    --       hdl <- Trace.activateContractWallet w1 c

    --       Trace.callEndpoint @"lock" hdl (Ada.lovelaceValueOf 10)
    --       void $ Trace.waitNSlots 1

    --       void $ Trace.payToWallet w2 w1 (value escrowParams)
    --       void $ Trace.waitNSlots 1

    --       Trace.callEndpoint @"redeem" hdl ()
    --       void $ Trace.waitNSlots 1


    -- , let c = void $ contract escrowParams in
    --   checkPredicate "cant redeem if party is not paid their due"
    --     ( assertDone c (Trace.walletInstanceTag w1) (const True) "escrow cant redeem not done"
    --     .&&. walletFundsChange w1 mempty
    --     .&&. walletFundsChange w2 mempty
    --     )
    --     $ do
    --       hdl <- Trace.activateContractWallet w1 c

    --       Trace.callEndpoint @"lock" hdl (Ada.lovelaceValueOf 10)
    --       void $ Trace.waitNSlots 1

    --       -- Don't pay.
    --       void $ Trace.waitNSlots 1

    --       Trace.callEndpoint @"redeem" hdl ()
    --       void $ Trace.waitNSlots 1
    ]


escrowParams :: EscrowParams
escrowParams =
  EscrowParams
    { party1    = pubKeyHash $ walletPubKey w1
    , redeemer  = pubKeyHash $ walletPubKey w2
    , value     = Ada.lovelaceValueOf 20
    , deadline  = 100
    }

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
