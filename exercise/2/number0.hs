
import Control.Monad.State (MonadState(..), State, execState)

number :: [String] -> State (Int, [String]) (Int, [String])
number =  rec
  where rec (i:is) =
          do (n, os) <- get
             put (n + 1 :: Int, os ++ [show n ++ ": " ++ i])
             rec is
        rec []     = get
  
main :: IO ()
main =  do lines' <- fmap lines getContents
           mapM_ putStrLn $ snd $
             execState (number lines') (1, [])
