{-# LANGUAGE LambdaCase #-}
{-|
A simple beta-reduction pass.

-}
module PlutusIR.Transform.Beta where

import PlutusPrelude

import PlutusIR

{-|
A single non-recursive application of the beta rule.

- TODO: check if annotations are applied correctly, and

-}

-- Local beta transformation
beta'
    :: Term tyname name uni fun a
    -> Term tyname name uni fun a
beta' = \case
    Apply a (LamAbs _ name typ body) arg -> 
        let varDecl  = VarDecl a name typ
            binding  = TermBind a Strict varDecl arg
            bindings = binding :| []
        in
            Let a NonRec bindings body
    t -> t

beta
    :: Term tyname name uni fun a
    -> Term tyname name uni fun a
beta t = beta' $ over termSubterms beta t
