module PeerManager where

import Data.ByteString                  (ByteString)
import Control.Concurrent
import Control.Concurrent.STM.TVar
import           Control.Concurrent.Async (race_)
import Messages
import Network                          (connectTo)
import Peer
import Tracker                          (Address(..))
import System.IO

createHandle :: Address -> IO Handle
createHandle a = connectTo (host a) (port a)

startPeer :: Address -> ByteString -> ByteString -> IO ()
startPeer addr ih pid = do
                 handle <- createHandle addr
                 hSetBinaryMode handle True
                 hSetBuffering handle LineBuffering
                 let peer = newPeer handle
                     hsTo = HandShake "BitTorrent protocol" 0 ih pid
                 sendHandShake hsTo peer
                 hsFrom <- getHandShake peer
                 print hsFrom
                 case validateHandShake hsTo hsFrom of
                      Left s -> print s
                      Right () -> do 
                                  sendMessage Interested peer
                                  tvPeer <- newTVarIO peer
                                  gTvBf <- newTVarIO (take 80 $ repeat False)
                                  race_ (listenToPeer tvPeer gTvBf)
                                        (requestStuff tvPeer gTvBf)

startPeers :: [Address] -> ByteString -> ByteString -> IO ()
startPeers peerList ih pid = startPeer (head . tail$ peerList) ih pid
          -- where handles = map createHandle peerList