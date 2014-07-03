module Peer (
         Peer(..)
       , Address(..)
       , newPeer
       , sendMsg
       ) where

import           Data.Binary
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import           Messages             (BitField, HandShake)
import           Tracker              (Address)
import           System.IO            (Handle)

data Peer = Peer { getHandle      :: Handle
                 , peerId         :: Maybe String 
                 , amInterested   :: Bool
                 , amChoking      :: Bool
                 , theyInterested :: Bool
                 , theyChoking    :: Bool
                 , haves          :: BitField
                 }
     deriving (Show)

newPeer :: Handle -> Peer
newPeer handle = Peer handle 
                      Nothing
                      False
                      False     
                      False
                      False
                      []

sendMsg :: HandShake -> Peer -> IO ()
sendMsg msg peer = do
                   let handle = getHandle peer
                   BL.hPut handle (runPut . put $ msg)