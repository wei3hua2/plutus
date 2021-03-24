{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Spec.Auction(tests, auctionTrace1, auctionTrace2) where

import           Control.Lens
import           Control.Monad             (void)
import           Data.Monoid               (Last (..))

import qualified Control.Monad.Freer       as Freer
import qualified Control.Monad.Freer.Error as Freer
import           Ledger                    (Ada, Value, pubKeyHash)
import qualified Ledger.Ada                as Ada
import           Plutus.Contract
import           Plutus.Contract.Test
import qualified Streaming.Prelude         as S
import qualified Wallet.Emulator.Folds     as Folds
import qualified Wallet.Emulator.Stream    as Stream

import           Ledger.Value              (Currency)
import qualified Ledger.Value              as Value
import           Plutus.Contracts.Auction
import qualified Plutus.Contracts.Currency as Currency
import qualified Plutus.Trace.Emulator     as Trace
import           PlutusTx.Monoid           (inv)

import           Test.Tasty

tests :: TestTree
tests =
    testGroup "auction"
        [ checkPredicateOptions options "run an auction"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertAccumState (buyer threadToken) (Trace.walletInstanceTag w2) ((==) trace1FinalState ) "final state should be OK"
            .&&. walletFundsChange w1 (Ada.toValue trace1WinningBid <> inv theToken <> Value.currencyValue threadToken 1)
            .&&. walletFundsChange w2 (inv (Ada.toValue trace1WinningBid) <> theToken))
            auctionTrace1
        , checkPredicateOptions options "run an auction with multiple bids"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w3) (const True) "3rd party should be done"
            .&&. assertAccumState (buyer threadToken) (Trace.walletInstanceTag w2) ((==) trace2FinalState) "final state should be OK"
            .&&. walletFundsChange w1 (Ada.toValue trace2WinningBid <> inv theToken <> Value.currencyValue threadToken 1)
            .&&. walletFundsChange w2 (inv (Ada.toValue trace2WinningBid) <> theToken)
            .&&. walletFundsChange w3 mempty)
            auctionTrace2
        ]

params :: AuctionParams
params =
    AuctionParams
        { apOwner   = pubKeyHash $ walletPubKey (Wallet 1)
        , apAsset   = theToken
        , apEndTime = 100
        }

-- | The token that we are auctioning off.
theToken :: Value
theToken =
    -- "ffff" is not a valid MPS hash. But this doesn't matter because we
    -- never try to forge any value of "ffff" using a script.
    -- This currency is created by the initial transaction.
    Value.singleton "ffff" "token" 1

-- | 'CheckOptions' that inclues 'theToken' in the initial distribution of wallet 1.
options :: CheckOptions
options =
    let initialDistribution = defaultDist & over (at (Wallet 1) . _Just) ((<>) theToken)
    in defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution

seller :: Contract AuctionOutput SellerSchema AuctionError ()
seller = auctionSeller (apAsset params) (apEndTime params)

buyer :: Currency -> Contract AuctionOutput BuyerSchema AuctionError ()
buyer cur = auctionBuyer cur params

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

trace1WinningBid :: Ada
trace1WinningBid = 50

auctionTrace1 :: Trace.EmulatorTrace ()
auctionTrace1 = do
    sellerHdl <- Trace.activateContractWallet w1 seller
    _ <- Trace.waitNSlots 3
    currency <- extractCurrency sellerHdl
    hdl2 <- Trace.activateContractWallet w2 (buyer currency)
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"bid" hdl2 trace1WinningBid
    void $ Trace.waitUntilSlot (succ $ succ $ apEndTime params)

trace2WinningBid :: Ada
trace2WinningBid = 70

extractCurrency :: Trace.ContractHandle AuctionOutput SellerSchema AuctionError -> Trace.EmulatorTrace Currency
extractCurrency handle = do
    t <- auctionThreadToken <$> Trace.observableState handle
    case t of
        Last (Just currency) -> pure currency
        _                    -> Trace.throwError (Trace.GenericError "currency not found")

auctionTrace2 :: Trace.EmulatorTrace ()
auctionTrace2 = do
    sellerHdl <- Trace.activateContractWallet w1 seller
    _ <- Trace.waitNSlots 3
    currency <- extractCurrency sellerHdl
    hdl2 <- Trace.activateContractWallet w2 (buyer currency)
    hdl3 <- Trace.activateContractWallet w3 (buyer currency)
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"bid" hdl2 50
    _ <- Trace.waitNSlots 15
    Trace.callEndpoint @"bid" hdl3 60
    _ <- Trace.waitNSlots 35
    Trace.callEndpoint @"bid" hdl2 trace2WinningBid
    void $ Trace.waitUntilSlot (succ $ succ $ apEndTime params)

trace1FinalState :: AuctionOutput
trace1FinalState =
    AuctionOutput
        { auctionState = Last $ Just $ Finished $ HighestBid
            { highestBid = trace1WinningBid
            , highestBidder = pubKeyHash (walletPubKey w2)
            }
        , auctionThreadToken = Last $ Just threadToken
        }

trace2FinalState :: AuctionOutput
trace2FinalState =
    AuctionOutput
        { auctionState = Last $ Just $ Finished $ HighestBid
            { highestBid = trace2WinningBid
            , highestBidder = pubKeyHash (walletPubKey w2)
            }
        , auctionThreadToken = Last $ Just threadToken
        }

threadToken :: Currency
threadToken =
    let con = Currency.createThreadToken @BlockchainActions @()
        fld = Folds.instanceOutcome con (Trace.walletInstanceTag w1)
        getOutcome (Folds.Done a) = a
        getOutcome e              = error $ "not finished: " <> show e
    in
    either (error . show) (getOutcome . S.fst')
        $ Freer.run
        $ Freer.runError @Folds.EmulatorFoldErr
        $ Stream.foldEmulatorStreamM fld
        $ Stream.takeUntilSlot 10
        $ Trace.runEmulatorStream (options ^. emulatorConfig)
        $ do
            void $ Trace.activateContractWallet w1 (void con)
            Trace.waitNSlots 3

