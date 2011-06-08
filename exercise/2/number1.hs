
import System.IO (isEOF)
import Data.IORef (newIORef, readIORef, modifyIORef)

number :: IO ()
number =  newIORef 1 >>= rec
  where rec counter = do eof <- isEOF
                         case eof of
                           False -> do l <- getLine
                                       n <- readIORef counter
                                       putStrLn $ show n ++ ": " ++ l
                                       modifyIORef counter (+1)
                                       rec counter
                           True  -> return ()

main :: IO ()
main = number
