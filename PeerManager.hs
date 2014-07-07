module PeerManager where

import           Data.ByteString                  (ByteString)
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Async         (race_)
import           Control.Exception                (bracket)
import           Control.Monad.STM                (atomically)
import           Messages
import           Network                          (connectTo, withSocketsDo)
import           Peer
import           Torrent
import           Tracker                          (Address(..))
import           System.IO                       

createHandle :: Address -> IO Handle
createHandle a = connectTo (host a) (port a)

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

-- withSocketsDo req for Windows; only adding for portability
startPeer :: Torrent -> Address -> IO ()
startPeer tor addr = do
  handle <- createHandle addr
  initAndRun tor handle
  return ()

startPeers :: [Address] -> Torrent -> IO ()
startPeers peerList tor = mapM_ (startPeer tor) [head peerList]
