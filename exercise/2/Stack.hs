module Stack (
  Stack(..),
  SimpleStack) where

import Prelude hiding (head, tail)

class Stack s where
  empty   :: s a
  isEmpty :: s a -> Bool
  cons    :: a -> s a -> s a
  head    :: s a -> a
  tail    :: s a -> s a

data SimpleStack a = SimpleStack { runSimpleStack :: [a] }

instance Stack SimpleStack where
  empty   = SimpleStack []
  isEmpty = null . runSimpleStack
  cons a  = SimpleStack . (a:) . runSimpleStack
  head (SimpleStack (e:_))  = e
  head (SimpleStack  [])    = error "head called against empty stack."
  tail (SimpleStack (_:es)) = SimpleStack es
  tail (SimpleStack  [])    = error "tail called against empty stack."
