module Tracker where

import           System.Environment
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Base16 as Base16
import           Crypto.Hash.SHA1       (hash)
import           Bencode                hiding (main)
import           Data.Char
import           Data.Word              (Word8)
import           Data.List              (intercalate)
import           Data.List.Split        (chunksOf)
import           Network                (PortID (..), HostName)
import           Peer                   (Address (Addr))

---- getters for metainfo
getInfo :: MetaInfo -> Maybe (BenValue, BenValue)
getInfo m = get (BenString "info") m

getPieces :: MetaInfo -> Maybe (BenValue, BenValue)
getPieces m = get (BenString "pieces") $ val (getInfo m)
          where val (Just (_,v)) = v

getAnnounceUrl :: MetaInfo -> String
getAnnounceUrl m = clean . extract $ get (BenString "announce") m
               where extract (Just (_, v)) = v
                     clean (BenString s) = filter (/= '"') s

getInfoHash :: MetaInfo -> B.ByteString
getInfoHash m = hash . B8.pack $ encoded
            where info = getInfo m
                  encoded = encodeOne $ extract info
                  extract (Just (k,v)) = v

--- components of the tracker GET request
peerIdHash :: B.ByteString
peerIdHash = hash . B8.pack $ ['a'..'Z']

uploaded :: String
uploaded = show 0

downloaded :: String
downloaded = show 0

toDownload :: MetaInfo -> String
toDownload m = show 0 --get (BenString "length") m - downloaded

-- | started | stopped | completed
event :: String
event = "started"

urlEncode :: B.ByteString -> String
urlEncode s = concat $ map helper hexs
    where hexs = bsChunksOf 2 $  Base16.encode s
          helper hex
               | hex `elem` allowed = B8.unpack . first $ Base16.decode hex
               | otherwise = '%' : B8.unpack hex           
          first (a,_) = a

allowed :: [B.ByteString]
allowed = bsChunksOf 2 $ Base16.encode reserved
        where reserved = B8.pack (['.', '-', '_', '~'] ++ nums ++ letters)
              nums = concat $ map show [0..9]
              letters = ['a'..'z'] ++ ['A'..'Z']

bsChunksOf :: Int -> B.ByteString -> [B.ByteString]
bsChunksOf n bs = map B.pack $ chunksOf n $ B.unpack bs

requestUrl :: MetaInfo -> String
requestUrl m = getAnnounceUrl m ++ "?" ++ 
              urlify [("info_hash", urlEncode . getInfoHash $ m),
                      ("peer_id", urlEncode peerIdHash),
                      ("port", show 6881),
                      ("uploaded", uploaded),
                      ("downloaded", downloaded),
                      ("left", toDownload m),
                      ("compact", "1"),
                      ("event", event)]
              where urlify = intercalate "&" . map paramVal
                    paramVal (p, v) = p ++ "=" ++ v

---- tracker response
getPeers :: MetaInfo -> String
getPeers m = extract $ get (BenString "peers") m 
         where extract (Just (_,(BenString s))) = s

peerList :: MetaInfo -> [Address]
peerList m = map processPeer $ chunksOf 6 word8s
         where word8s = B.unpack . B8.pack . getPeers $ m


processPeer :: [Word8] -> Address
processPeer raw = Addr host (PortNumber (x*256 + y))
            where (ip, port) = splitAt 4 raw
                  host = intercalate "." $ map show ip
                  (x:y:[]) = map fromIntegral port

processResponse :: String -> [Address]
processResponse s = peerList . parseOne . B8.pack $ s
