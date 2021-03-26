{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | On-chain code fragments for creating a state machine. First
--   define a @StateMachine s i@ with input type @i@ and state type @s@. Then
--   use 'mkValidator' in on-chain code to check the required hashes and
--   validate the transition, and 'mkRedeemer' to make redeemer scripts.
module Plutus.Contract.StateMachine.OnChain(
      StateMachine(..)
    , StateMachineInstance (..)
    , WithAssetClass(..)
    , threadTokenValue
    , State(..)
    , mkStateMachine
    , machineAddress
    , mkValidator
    ) where

import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Void                        (Void)
import           GHC.Generics                     (Generic)

import           Ledger.Constraints
import           Ledger.Constraints.TxConstraints (OutputConstraint (..))
import qualified Ledger.Tokens                    as Tokens
import qualified PlutusTx                         as PlutusTx
import           PlutusTx.Prelude                 hiding (check)

import           Ledger                           (Address, Value)
import           Ledger.Contexts                  (TxInInfo (..), ValidatorCtx (..), findOwnInput)
import           Ledger.Typed.Scripts
import           Ledger.Value                     (AssetClass, isZero)
import qualified Prelude                          as Haskell

data WithAssetClass s =
    WithAssetClass
        { assetClass :: Maybe AssetClass
        , s          :: s
        }

data State s = State { stateData :: s, stateValue :: Value }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Specification of a state machine, consisting of a transition function that determines the
-- next state from the current state and an input, and a checking function that checks the validity
-- of the transition in the context of the current transaction.
data StateMachine s i = StateMachine {
      -- | The transition function of the state machine. 'Nothing' indicates an invalid transition from the current state.
      smTransition :: State s -> i -> Maybe (TxConstraints Void Void, State s),

      -- | Check whether a state is the final state
      smFinal      :: s -> Bool,

      -- | The condition checking function. Can be used to perform
      --   checks on the pending transaction that aren't covered by the
      --   constraints. 'smCheck' is always run in addition to checking the
      --   constraints, so the default implementation always returns true.
      smCheck      :: s -> i -> ValidatorCtx -> Bool
    }

{-# INLINABLE threadTokenValue #-}
-- | The 'Value' containing exactly the thread token, if one has been specified.
threadTokenValue :: Maybe AssetClass -> Value
threadTokenValue assetClass = maybe mempty Tokens.token assetClass

-- | A state machine that does not perform any additional checks on the
--   'ValidatorCtx' (beyond enforcing the constraints)
mkStateMachine
    :: (State s -> i -> Maybe (TxConstraints Void Void, State s))
    -> (s -> Bool)
    -> StateMachine s i
mkStateMachine smTransition smFinal =
    StateMachine
        { smTransition
        , smFinal
        , smCheck = \_ _ _ -> True
        }

instance ScriptType (StateMachine s i) where
    type instance RedeemerType (StateMachine s i) = i
    type instance DatumType (StateMachine s i) = WithAssetClass s

data StateMachineInstance s i = StateMachineInstance {
    -- | The state machine specification.
    stateMachine      :: StateMachine s i,
    -- | The validator code for this state machine.
    validatorInstance :: ScriptInstance (StateMachine s i),
    -- | The thread token (if it exists) for this state machine instance
    threadToken       :: Maybe AssetClass
    }

machineAddress :: StateMachineInstance s i -> Address
machineAddress = scriptAddress . validatorInstance

{-# INLINABLE mkValidator #-}
-- | Turn a state machine into a validator script.
mkValidator :: forall s i. (PlutusTx.IsData s) => StateMachine s i -> ValidatorType (StateMachine s i)
mkValidator (StateMachine step isFinal check) WithAssetClass{assetClass, s = currentState} input ptx =
    let vl = txInInfoValue (findOwnInput ptx)
        checkOk = traceIfFalse "State transition invalid - checks failed" (check currentState input ptx)
        oldState = State{stateData=currentState, stateValue=vl}
        stateAndOutputsOk = case step oldState input of
            Just (newConstraints, State{stateData=newData, stateValue=newValue})
                | isFinal newData ->
                    traceIfFalse "Non-zero value allocated in final state" (isZero newValue)
                    && traceIfFalse "State transition invalid - constraints not satisfied by ValidatorCtx" (checkValidatorCtx newConstraints ptx)
                | otherwise ->
                    let newState' = WithAssetClass{assetClass, s = newData}
                        txc =
                            newConstraints
                                { txOwnOutputs=
                                    [ OutputConstraint
                                        { ocDatum = newState'
                                        , ocValue = newValue <> threadTokenValue assetClass
                                        }
                                    ]
                                }
                    in traceIfFalse "State transition invalid - constraints not satisfied by ValidatorCtx" (checkValidatorCtx @_ @s (fmap s txc) ptx)
            Nothing -> trace "State transition invalid - input is not a valid transition at the current state" False
    in checkOk && stateAndOutputsOk

PlutusTx.makeLift ''WithAssetClass
PlutusTx.makeIsDataIndexed ''WithAssetClass [('WithAssetClass, 0)]
