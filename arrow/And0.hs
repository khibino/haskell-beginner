{-# LANGUAGE Arrows #-}

module And where

import Prelude hiding(id, (.))
import CircuitDef

and2 :: (Bool, Bool) -> Bool
and2 = uncurry (&&)

and' :: Auto (Bool, Bool) Bool
and' = proc (i0, i1) -> do
         o0 <- arr and2 -< (i0, i1)
         id -< o0

and'' :: Auto (Bool, Bool) Bool
and'' = proc (i0, i1) -> do
         arr and2 -< (i0, i1)

and''' :: Auto (Bool, Bool) Bool
and''' =  arr and2
