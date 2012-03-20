import Libertree.Backend.Server
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  _ <- forkIO $ startServer
  putStrLn "Hit any key to exit.\n"
  getLine >> return ()

