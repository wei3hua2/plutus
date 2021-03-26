{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Spec.SimpleEscrow(tests) where

import           Control.Lens
import           Control.Monad                 (void)

import           Ledger                        (Value, pubKeyHash)
import qualified Ledger.Ada                    as Ada
import           Plutus.Contract
import           Plutus.Contract.Test

import           Plutus.Contracts.SimpleEscrow (EscrowParams (..), contract, lockEp, redeemEp, refundEp)

import qualified Ledger.Value                  as Value
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
          Trace.callEndpoint @"lock" hdl ()
          void $ Trace.waitNSlots 1


    , checkPredicate "can lock and refund"
        ( walletFundsChange w1 mempty
        )
        $ do
          let c = void $ Plutus.Contract.both (lockEp escrowParams) (refundEp escrowParams)

          hdl <- Trace.activateContractWallet w1 c
          Trace.callEndpoint @"lock" hdl ()
          void $ Trace.waitNSlots 1

          -- Note: The way we've set up the refund is that it only runs
          -- validation after the deadline; so we need to wait until then to
          -- check that it was successful.
          Trace.callEndpoint @"refund" hdl ()
          void $ Trace.waitUntilSlot (succ $ deadline escrowParams)


    , checkPredicate "(Ada) can redeem if party is paid their due"
        (    walletFundsChange w1 (Ada.lovelaceValueOf (-7))
        .&&. walletFundsChange w2 (Ada.lovelaceValueOf 7)
        )
        $ do
         -- payee     = w1
         --   expecting: 2 LL
         --   paying:    9 LL
          let params = escrowParams { expecting = Ada.lovelaceValueOf 2
                                    , paying    = Ada.lovelaceValueOf 9 }


          void $ Trace.payToWallet w2 w1 (expecting params)
          void $ Trace.waitNSlots 2

          let c = void $ contract params

          hdl <- Trace.activateContractWallet w1 c

          Trace.callEndpoint @"lock" hdl ()
          void $ Trace.waitNSlots 1

          Trace.callEndpoint @"redeem" hdl ()

          void $ Trace.waitUntilSlot (succ $ deadline params)


      -- Deal: Exchange 10 Token1 for 5 Token2
      --
      --  W1: Pays 10 Token1, Receives  5 Token2
      --  W2: Pays  5 Token2, Receives 10 Token1
    , checkPredicateOptions options "can perform exchange"
        (    walletFundsChange w1 (token1 (-10) <> token2 5)
        .&&. walletFundsChange w2 (token2 (-5)  <> token1 10)
        )
        $ do
          -- payee     = w1
          --   expecting: 5  Token2
          --   paying:    10 Token1
          --
          -- recipient = w2
          --  => receive 10 of Token 1
          --  => pay 5 token 2
          let params = escrowParams { expecting = token2 5
                                    , paying    = token1 10 }

          let c = void $ contract params

          hdl <- Trace.activateContractWallet w1 c

          Trace.callEndpoint @"lock" hdl ()
          void $ Trace.waitNSlots 10

          -- Recipient pays to Payee 5 Token2
          void $ Trace.payToWallet w2 w1 (expecting params)
          void $ Trace.waitNSlots 10

          -- Pay 10 of Token1 to Recipient
          Trace.callEndpoint @"redeem" hdl ()

          void $ Trace.waitUntilSlot (succ $ deadline params)


    , checkPredicateOptions options "refund if party is not paid their due by deadline"
        ( walletFundsChange w1 mempty
        )
        $ do
          let params = escrowParams { expecting = token2 100
                                    , paying    = token1 10
                                    }

          let c = void $ contract params
          hdl <- Trace.activateContractWallet w1 c

          Trace.callEndpoint @"lock" hdl ()

          -- Don't pay.
          void $ Trace.waitUntilSlot (succ $ deadline escrowParams)
          void $ Trace.waitNSlots 1
          -- TODO: This doesn't result in refund being called; I'm not sure
          -- why.
          Trace.callEndpoint @"refund" hdl ()
          void $ Trace.waitNSlots 2
    ]


token1 :: Integer -> Value
token1 = Value.singleton "1111" "Token1"

token2 :: Integer -> Value
token2 = Value.singleton "2222" "Token2"

options :: CheckOptions
options =
    let initialDistribution = defaultDist & over (at w1 . _Just) ((<>) (token1 500))
                                          & over (at w2 . _Just) ((<>) (token2 500))
    in defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution

options2 :: CheckOptions
options2 =
    let initialDistribution = defaultDist & over (at w1 . _Just) ((<>) (token1 500))
                                          & over (at w2 . _Just) ((<>) (token1 500))
    in defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution


escrowParams :: EscrowParams
escrowParams =
  EscrowParams
    { payee     = pubKeyHash $ walletPubKey w1
    , recipient = pubKeyHash $ walletPubKey w2
    , expecting = Ada.lovelaceValueOf 20
    , paying    = Ada.lovelaceValueOf 10
    , deadline  = 100
    }

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
