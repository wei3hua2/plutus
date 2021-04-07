-- | Various IR optimizations

module PlutusIR.Optimizer.Simplifier where

-- TODO Maybe we should just call this simplifier?


none :: Term TyName Name uni fun b -> Term TyName Name uni fun b
none = identity

-- | Perform some simplification of a 'Term'.
basic :: Term TyName Name uni fun b -> Term TyName Name uni fun b
basic = Inline.inline . DeadCode.removeDeadBindings

