module Main where

import           Bencode
import           Control.Applicative    ((<$>))
import           Data.Binary            (get)
import           Data.Binary.Get        (runGet)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8       
import qualified Data.ByteString.Lazy   as BL
import           Data.Monoid            ((<>))
import           Messages               
import           Network                (connectTo)
import           Network.HTTP           ( simpleHTTP
                                        , getRequest
                                        , getResponseBody)

import           Peer
import           Tracker
import           System.Environment     (getArgs)
import           System.IO              (Handle, hSetBuffering, BufferMode(..), hSetBinaryMode)

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
                     hshake = HandShake "BitTorrent protocol" ih pid
                 sendMsg hshake peer
                 bs <- BL.hGet handle 68
                 let peerHs = (runGet get bs :: HandShake)
                 print $ (infoHash peerHs) == ih
                 let loop = do
                     -- len <- BL.hGet handle 4
                     -- let intlen = bsToInt len
                     -- print intlen
                     -- if intlen == 0
                     --    then loop
                     --    else do
                     --         msg <- BL.hGet handle intlen
                 --             print $ (runGet get (len <> msg) :: Message)
                 --             loop
                      msg <- BL.hGetContents handle
                      print $ (runGet get msg :: Message)
                      loop
                 loop
            where bsToInt = extract . B8.readInt . BL.toStrict
                  extract Nothing = 0
                  extract (Just (i, _)) = i

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
