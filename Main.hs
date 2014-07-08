module Main where

import           Control.Applicative    ((<$>))
import           Control.Concurrent.STM (newTVarIO, atomically)
import           Data.Binary            (get)
import           Data.Binary.Get        (runGet)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8       
import qualified Data.ByteString.Lazy   as BL
import           Data.Monoid            ((<>))
import           Data.Word
import           System.Environment     (getArgs)
import           System.IO              ( Handle 
                                        , hSetBuffering
                                        , BufferMode(..)        
                                        , hSetBinaryMode
                                        )

import           Network.HTTP           ( simpleHTTP
                                        , getRequest
                                        , getResponseBody
                                        )

import           Bencode
import           Messages               
import           Peer
import           PeerManager
import           Torrent
import           Tracker

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
        has Nothing _       = False
        has _       Nothing = False
        has _       _       = True

getMetaInfo :: String -> IO (Maybe MetaInfo)
getMetaInfo fn = do
    meta <- parseOne <$> B.readFile fn
    case validMeta meta of
        True  -> return (Just meta)
        False -> return Nothing

main :: IO ()
main = do
    fileName <- getFilePath
    maybeMeta <- getMetaInfo fileName
    case maybeMeta of
        Nothing   -> fail "Invalid contents of .torrent file"
        Just meta -> if multiFile meta 
                       then print "Sorry, we do not support multi-file torrents"
                       else do 
                         peerList <- announceTracker meta   
                         tor <- atomically $ newTorrent meta peerIdHash
                         print $ peerList
                         print $ numPieces tor
                         startPeers peerList tor
  where multiFile meta = case getFiles meta of
                           Nothing -> False
                           Just _ -> True
        getFiles meta = getFromDict (BenString "files") (val $ getInfo meta)
        val (Just (_,v)) = v