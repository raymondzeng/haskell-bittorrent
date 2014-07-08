module PeerManager where

import           Data.ByteString                  (ByteString)
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Async         (race_)
import           Control.Exception                
import           Control.Monad                    (forM_, forever)
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

startPeer tor addr = forkFinally start handleError
  where start = bracket (createHandle addr) 
                        (\h -> print "Closing Connection" >> hClose h) 
                        (initAndRun tor) 
        handleError (Left e) = print (show e)
        handleError (Right ()) = print "Thread Done."

startPeers :: [Address] -> Torrent -> IO ()
startPeers peerList tor = do
    mapM_ (startPeer tor) peerList 
    forever $ threadDelay 99999999999
