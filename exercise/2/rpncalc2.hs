
import Prelude hiding (head, tail, div, drop)
import Stack (Stack(..), SimpleStack)
import Control.Monad.State (MonadState(..), State, evalState)

type CalcState = SimpleStack Float

dump :: CalcState -> String
dump =  (++ "----------\n") . ("Stack Dump:\n" ++) . rec
  where rec stack =
          if isEmpty stack then ""
          else show (head stack) ++ "\n" ++ rec (tail stack)

op2 :: (Float -> Float -> Float) -> CalcState -> CalcState
op2 op stack = cons (head stack `op` head sTail) (tail sTail)
  where sTail = tail stack

plus = op2 (+)
subt = op2 (-)
mult = op2 (*)
div  = op2 (/)

clear  = const empty
drop   = tail
swap stack = cons (head sTail) (cons (head stack) (tail sTail))
  where sTail = tail stack

plus, subt, mult, div, clear, drop, swap :: CalcState -> CalcState

display :: CalcState -> String
display =  (++ "\n") . show . head


calcStep :: CalcState -> String -> (CalcState, String)
calcStep stack = proc
  where pure fun = (fun stack, "")
        proc "+" = pure plus
        proc "-" = pure subt
        proc "*" = pure mult
        proc "/" = pure div
        proc "clear" = pure clear
        proc "drop"  = pure drop
        proc "swap"  = pure swap
        proc "display" = (stack, display stack)
        proc "debugDump" = (stack, dump stack)
        proc num     = pure (cons (read num :: Float))

calc :: String -> State CalcState String
calc word =
  do st0 <- get
     let (st1, out) = calcStep st0 word
     put st1
     return out

main :: IO ()
main =  interact $ (`evalState` empty) . fmap concat . mapM calc . lines

