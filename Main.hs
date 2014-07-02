module Main where

import           Bencode
import           Control.Applicative    ((<$>))
import           Data.Binary            (get)
import           Data.Binary.Get        (runGet)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import           Data.ByteString.Lazy   (fromChunks)
import           Messages               
import           Network                (connectTo)
import           Network.HTTP           ( simpleHTTP
                                        , getRequest
                                        , getResponseBody)

import           Peer
import           Tracker
import           System.Environment     (getArgs)
import           System.IO              (Handle, hSetBuffering, BufferMode(..))

getFilePath :: IO String
getFilePath = do
            args <- getArgs
            case args of  
                 []    -> fail "No file given"
                 (x:_) -> return x

validMeta :: MetaInfo -> Bool
validMeta m = has info announce
          where info = getFromDict (BenString "info") m
                announce = getFromDict (BenString "announce") m
                has Nothing _ = False
                has _ Nothing = False
                has _ _ = True

getMetaInfo :: String -> IO (Maybe MetaInfo)
getMetaInfo fn = do
            meta <- parseOne <$> B.readFile fn
            if (validMeta meta)
               then return (Just meta)
               else return Nothing

createHandle :: Address -> IO Handle
createHandle a = connectTo (host a) (port a)

startPeer :: Address -> ByteString -> ByteString -> IO Handle
startPeer addr ih pid = do
                 handle <- createHandle addr
                 hSetBuffering handle LineBuffering
                 let peer = newPeer handle
                     hshake = HandShake "BitTorrent protocol" ih pid
                 sendMsg hshake peer
                 return handle

main :: IO ()
main = do
     fileName <- getFilePath
     maybeMeta <- getMetaInfo fileName
     case maybeMeta of
          Nothing   -> fail "Invalid contents of .torrent file"
          Just meta -> do 
                  peerList <- announceTracker meta   
                  let infoHash = getInfoHash meta
                  print (head peerList)
                  handle <- startPeer (head peerList) infoHash peerIdHash
                  bs <-  B.hGet handle 100
                  print $ (runGet get (fromChunks [bs]) :: HandShake)