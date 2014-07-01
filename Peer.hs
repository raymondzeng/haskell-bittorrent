module Peer where

import Network          (PortID (..), HostName)
import System.IO        (Handle)


data Address = Addr { host :: HostName
                    , port :: PortID
                    }
     deriving (Show)

instance Show a => Show (IO a) where
         show _ = "IO Handle"

data Peer = Peer { peerHandle     :: IO Handle
                 , peerId         :: Maybe String 
                 , amInterested   :: Bool
                 , amChoking      :: Bool
                 , theyInterested :: Bool
                 , theyChoking    :: Bool
                 }
     deriving (Show)

newPeer :: IO Handle -> Peer
newPeer handle = Peer handle 
                      Nothing
                      False
                      False     
                      False
                      False
