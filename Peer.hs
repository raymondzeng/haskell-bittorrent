module Peer (
         Peer(..)
       , Address(..)
       , newPeer
       , sendHandShake
       , getHandShake
       , validateHandShake
       , sendMessage
       , listenPeer
       ) where

import           Control.Monad        (forever)
import           Data.Binary          as Bin
import           Data.Binary.Put
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid          ((<>))
import           Messages             (BitField, HandShake(..), Message(..))
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

sendHandShake :: HandShake -> Peer -> IO ()
sendHandShake hs peer = BL.hPut handle (runPut . put $ hs)
              where handle = getHandle peer

getHandShake :: Peer -> IO HandShake
getHandShake peer = do
                  let handle = getHandle peer
                  byte <- BL.hGet handle 1
                  let mlen = fromIntegral $ BL.head byte
                  msg <- BL.hGet handle (mlen + 48)
                  return (runGet get (byte <> msg) :: HandShake)
                  
-- TODO : verify peer id
validateHandShake :: HandShake -> HandShake -> Either String ()
validateHandShake to@(HandShake _ _ tih _) from@(HandShake fport _ fih _)
    | fport /= "BitTorrent protocol" = Left ("Peer sent HS with bad protocol: " ++ fport)
    | fih /= tih = Left "Peer's HS info hash does not match"
    | otherwise = Right ()

sendMessage :: Message -> Peer -> IO ()
sendMessage msg peer = BL.hPut handle (runPut . put $ msg)
            where handle = getHandle peer

listenPeer peer = forever $ do
                         bytes <- BL.hGet handle 4
                         let mlen = toInt bytes
                         msg <- BL.hGet handle mlen
                         let m = (runGet get (bytes <> msg) :: Message)
                         print m
                  --       sendMessage (Request 0 0 1000) peer
                where toInt len = fromIntegral (Bin.decode $ len :: Word32)
                      handle = getHandle peer 