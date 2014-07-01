import System.IO
import Network.Socket 
import Network as N
import Text.Printf
import Control.Concurrent
import Control.Monad 

talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering                                -- 1
  loop                                                         -- 2
 where
  loop = do
    line <- hGetLine h                                         -- 3
    if line == "end"                                           -- 4
       then hPutStrLn h ("Thank you for using the " ++         -- 5
                         "Haskell doubling service.")
       else do hPutStrLn h (show (2 * (read line :: Integer))) -- 6
               loop                                            -- 7


main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))              -- 1
  printf "Listening on port %d\n" port
  forever $ do                                                   -- 2
     (handle, host, port) <- N.accept sock                         -- 3
     printf "Accepted connection from %s: %s\n" host (show port)
     forkFinally (talk handle) (\_ -> hClose handle)             -- 4

port :: Int
port = 4444