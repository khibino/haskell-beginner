
import Prelude hiding (head, tail, div, drop)
import System.IO (hFlush, stdout)
import Stack (Stack(..), SimpleStack)
import Control.Monad.State (MonadState(..), State, execState)

type CalcState = SimpleStack Float

toIO :: a -> IO a
toIO =  return

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

display :: CalcState -> IO ()
display =  print . head



calcStep :: CalcState -> String -> (CalcState, IO ())
calcStep stack = proc
  where pure fun = (fun stack, toIO ())
        proc "+" = pure plus
        proc "-" = pure subt
        proc "*" = pure mult
        proc "/" = pure div
        proc "clear" = pure clear
        proc "drop"  = pure drop
        proc "swap"  = pure swap
        proc "display" = (stack, display stack)
        proc "debugDump" = (stack, putStr (dump stack) >> hFlush stdout)
        proc num     = pure (cons (read num :: Float))

type CalcAction = (CalcState, [IO ()])

calc :: [String] -> State CalcAction CalcAction
calc lines' = rec $ concatMap words lines'
  where rec (w:ws) = do (st0, acts) <- get
                        let (st1, act) = calcStep st0 w in
                          put (st1, acts ++ [act])
                        rec ws
        rec []     = get
    
main :: IO ()
main = do lines' <- fmap lines getContents
          sequence_ $ snd $ execState (calc lines') (empty, [])
