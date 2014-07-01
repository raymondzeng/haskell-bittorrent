module Peer where

import Network (PortID (..), HostName)

data Address = Addr { host :: HostName
                    , port :: PortID
                    }
     deriving (Show)

data Peer = Peer { peerId       :: Maybe String 
                 , amInterested :: Bool
                 , amChoking    :: Bool
                 , theyInterested :: Bool
                 , theyChoking    :: Bool
                 }
     deriving (Show)

