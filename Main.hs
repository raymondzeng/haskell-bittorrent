module Main where

import           Bencode
import qualified Data.Binary                 as BIN
import           Control.Applicative    ((<$>))
import           Data.Binary            (get)
import           Data.Binary.Get        (runGet)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8       
import qualified Data.ByteString.Lazy   as BL
import           Data.Monoid            ((<>))
import           Data.Word
import           Messages               
import           Network                (connectTo)
import           Network.HTTP           ( simpleHTTP
                                        , getRequest
                                        , getResponseBody)

import           Peer
import           Tracker
import           System.Environment     (getArgs)
import           System.IO              ( Handle 
                                        , hSetBuffering
                                        , BufferMode(..)        
                                        , hSetBinaryMode)

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

startPeer :: Address -> ByteString -> ByteString -> IO ()
startPeer addr ih pid = do
                 handle <- createHandle addr
                 hSetBinaryMode handle True
                 hSetBuffering handle LineBuffering
                 let peer = newPeer handle
                     hshake = HandShake "BitTorrent protocol" (B8.pack "00000000") ih pid
                 sendMsg hshake peer
                 bs <- BL.hGet handle 1
                 let intlen = fromIntegral $ BL.head bs
                 msg <- BL.hGet handle (intlen + 48)
                 let peerHs = (runGet get (bs <> msg) :: HandShake)
                 print peerHs
                 len <- BL.hGet handle 4
                 print len
                 let intlen = w4ToInt len
                 print intlen
                 msg <- BL.hGet handle intlen
                 print msg
                 let m = (runGet get (len <> msg) :: Message)
                 print m
             where w4ToInt len = fromIntegral (BIN.decode $ len :: Word32)

main :: IO ()
main = do
     fileName <- getFilePath
     maybeMeta <- getMetaInfo fileName
     case maybeMeta of
          Nothing   -> fail "Invalid contents of .torrent file"
          Just meta -> do 
                  peerList <- announceTracker meta   
                  let ih = getInfoHash meta
                  print (head peerList)
                  startPeer (head peerList) ih peerIdHash
