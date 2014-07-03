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
import           Data.Word
import           Messages               
import           Network.HTTP           ( simpleHTTP
                                        , getRequest
                                        , getResponseBody
                                        )

import           Peer
import           PeerManager
import           Tracker
import           System.Environment     (getArgs)
import           System.IO              ( Handle 
                                        , hSetBuffering
                                        , BufferMode(..)        
                                        , hSetBinaryMode
                                        )

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
            if (validMeta meta)
               then return (Just meta)
               else return Nothing

main :: IO ()
main = do
     fileName <- getFilePath
     maybeMeta <- getMetaInfo fileName
     case maybeMeta of
          Nothing   -> fail "Invalid contents of .torrent file"
          Just meta -> do 
                  peerList <- announceTracker meta   
                  let ih = getInfoHash meta
                  startPeers peerList ih peerIdHash
