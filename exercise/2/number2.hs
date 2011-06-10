
import Control.Monad.State (MonadState(..), State, evalState, modify)

number :: String -> State Int String
number line =
  do modify (+1)
     fmap ((++ ": " ++ line) . show) get

main :: IO ()
main =  interact $ (`evalState` 0) . fmap unlines . mapM number . lines
