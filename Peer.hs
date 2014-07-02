module Peer (
         Peer(..)
       , Address(..)
       , newPeer
       ) where

import Network          (PortID (..), HostName)
import System.IO        (Handle)


data Address = Addr { host :: HostName
                    , port :: PortID
                    }
     deriving (Show)

data Peer = Peer { getHandle      :: Handle
                 , peerId         :: Maybe String 
                 , amInterested   :: Bool
                 , amChoking      :: Bool
                 , theyInterested :: Bool
                 , theyChoking    :: Bool
                 }
     deriving (Show)

newPeer :: Handle -> Peer
newPeer handle = Peer handle 
                      Nothing
                      False
                      False     
                      False
                      False