module PeerManager where

import           Control.Applicative              ((<$>))
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Async         (race_)
import           Control.Exception                
import           Control.Monad                    (forM_, forever)
import           Control.Monad.STM                (atomically)
import           Data.ByteString                  (ByteString)
import           Network                          (connectTo, withSocketsDo)
import           System.IO                       

import           Messages
import           Peer
import           Torrent
import           Tracker                          (Address(..))

createHandle :: Address -> IO Handle
createHandle addr = do
  print $ "Connecting to " ++ show addr ++ "..."
  handle <- connectTo (host addr) (port addr) 
  print $ "Connected to : " ++ show (host addr) ++ show (port addr)
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
            race_ (listenToPeer tor peer)
                  (talkToPeer tor peer)

startPeer tor addr = forkFinally start handleError
  where start = bracket (createHandle addr) 
                        (\h -> print ("Closing Connection for : " ++ show addr) >> hClose h) 
                        (initAndRun tor) 
        handleError (Left e) = print $ "Thread failed for : " ++ show addr ++ " because " ++ (show e)
        handleError (Right ()) = print "Thread Done."

startPeers :: [Address] -> Torrent -> IO ()
startPeers peerList tor = do
    mapM_ (startPeer tor) peerList
          -- TODO change this to while children running
    whileM_ (notComplete) $ threadDelay 100000
  where notComplete = not <$> readTVarIO (done tor)