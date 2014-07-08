module PeerManager where

import           Data.ByteString                  (ByteString)
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Async         (race_)
import           Control.Exception                
import           Control.Monad                    (forM_)
import           Control.Monad.STM                (atomically)
import           Messages
import           Network                          (connectTo, withSocketsDo)
import           Peer
import           Torrent
import           Tracker                          (Address(..))
import           System.IO                       

createHandle :: Address -> IO Handle
createHandle a = do
  handle <- connectTo (host a) (port a) 
  print $ "Connected to : " ++ show (host a) ++ show (port a)
  return handle

initAndRun :: Torrent -> Handle -> IO ()
initAndRun tor handle = do
    hSetBinaryMode handle True
    peer <- atomically . newPeer $ handle
    let ih = Torrent.infoHash tor
        pid = myId tor
        hsTo = HandShake "BitTorrent protocol" 0 ih pid
    sendHandShake hsTo peer
    hsFrom <- getHandShake peer
    print hsFrom
    case validateHandShake hsTo hsFrom of
        Left s -> print s
        Right () -> do 
            race_ (listenToPeer peer tor)
                  (requestStuff peer tor)

-- I would like to be able to attempt to connectTo on a separate thread, but dont think i can
startPeer :: Torrent -> Address -> IO ()
startPeer tor addr = do 
  res <- try (createHandle addr)
  case res of
    Left e -> print $ "Failed to connect to : " ++ show addr ++ show (e :: SomeException)
    Right h -> (forkIO $ initAndRun tor h )>> return ()

  -- start handle
  -- where start handle = forkFinally (initAndRun tor handle) (handleError)
  --       handleError (Left e) = print $ show e
  --       handleError (Right ()) = print "thread Done"

--   where start = bracket (createHandle addr) (\h -> print "Closing" >> hClose h) (\_ -> print "helloooo") --(initAndRun tor) 
--     where    handleError (Left e) = print (show e)
--              handleError (Right ()) = print "thread done"

startPeers :: [Address] -> Torrent -> IO ()
startPeers peerList tor = mapM_ (startPeer tor) peerList 
