module PeerManager where

import           Data.ByteString                  (ByteString)
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Async         (race_)
import           Control.Exception                (bracket)
import           Messages
import           Network                          (connectTo, withSocketsDo)
import           Peer
import           Torrent
import           Tracker                          (Address(..))
import           System.IO                       

createHandle :: Address -> IO Handle
createHandle a = connectTo (host a) (port a)

initAndRun :: TVar Torrent -> Handle -> IO ()
initAndRun tTor handle = do
    hSetBinaryMode handle True
    tor <- readTVarIO tTor
    let peer = newPeer handle
        ih = Torrent.infoHash tor
        pid = myId tor
        hsTo = HandShake "BitTorrent protocol" 0 ih pid
    sendHandShake hsTo peer
    hsFrom <- getHandShake peer
    print hsFrom
    case validateHandShake hsTo hsFrom of
        Left s -> print s
        Right () -> do 
            tvPeer <- newTVarIO peer
            globalHaves <- newTVarIO (take 80 $ repeat False)
            race_ (listenToPeer tvPeer globalHaves)
                  (requestStuff tvPeer tTor)

-- withSocketsDo req for Windows; only adding for portability
startPeer :: TVar Torrent -> Address -> IO ()
startPeer tTor addr = do
  handle <- createHandle addr
  forkIO $ initAndRun tTor handle
  return ()

startPeers :: [Address] -> TVar Torrent -> IO ()
startPeers peerList tTor = mapM_ (startPeer tTor) peerList
