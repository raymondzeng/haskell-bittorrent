module PeerManager where

import           Data.ByteString                  (ByteString)
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Async         (race_)
import           Control.Exception                (bracket, SomeException)
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
  print (host a)
  handle <- connectTo (host a) (port a) 
  print "made handle"
  return handle

initAndRun :: Torrent -> Handle -> IO ()
initAndRun tor handle = do
    print "hello"
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

startPeer tor addr = do
  handle <- createHandle addr
  forkIO $ initAndRun tor handle
  return ()
-- startPeer :: Torrent -> Address -> IO ThreadId
-- startPeer tor addr = forkFinally start handleError
--   where start = bracket (createHandle addr) (hClose) (\h -> initAndRun tor h)
--         handleError (Left e) = print (show e)
--         handleError (Right ()) = print "Done"

startPeers :: [Address] -> Torrent -> IO ()
startPeers peerList tor = mapM_ (startPeer tor) peerList >> print "Done"
