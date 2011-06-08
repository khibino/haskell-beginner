
import Prelude hiding (head, tail, div, drop)
import System.IO (isEOF, hFlush, stdout)
import Data.IORef
import Control.Monad(when)
import Stack (Stack(..), SimpleStack)

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

calc :: IO ()
calc = newIORef empty >>= run
  where run stateRef = loop
          where loop = do eof <- isEOF
                          when (not eof) $
                            do line <- getLine
                               runWords (words line)
                               loop
                runWords (w:ws) = do state0 <- readIORef stateRef
                                     let (state1, act) = calcStep state0 w in
                                       do writeIORef stateRef state1
                                          act 
                                          runWords ws
                runWords  []    = toIO ()

main :: IO ()
main =  calc
