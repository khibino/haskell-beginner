
module CircuitDef (
  id, arr, (.),
  Auto (..),
  toAutomaton,
  toAuto) where

import Prelude hiding (id, (.))
import Control.Category (id, (.))
import Control.Arrow (ArrowLoop, arr, (>>>), (<<<), (&&&))
import Control.Arrow.Transformer (lift)
import Control.Arrow.Operations (ArrowCircuit, delay)
import Control.Arrow.Transformer.Automaton (Automaton, runAutomaton)
import qualified Data.Stream as S

toAutomaton :: ArrowLoop a => a i o -> Automaton a i o
toAutomaton =  lift

type Auto i o = Automaton (->) i o

toAuto :: (i -> o) -> Auto i o
toAuto =  toAutomaton
