module Tracker where

import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as Base16
import Crypto.Hash.SHA1 (hash)
import Bencode hiding (main)
import Data.Char
import Data.List (intercalate)
import Data.List.Split (chunksOf)

getInfo :: MetaInfo -> Maybe (BenValue, BenValue)
getInfo m = get info m
        where info = BenString "info"

getPieces :: MetaInfo -> Maybe (BenValue, BenValue)
getPieces m = get pieces $ val (getInfo m)
         where pieces = BenString "pieces"
               val (Just (_,v)) = v

-- getLength m = get length $ val (getInfo m)
--           where length = BenString "piece length"
--                 val (Just (_,v)) = v

getAnnounceUrl :: MetaInfo -> String
getAnnounceUrl m = clean . extract $ get announce m
               where announce = BenString "announce"
                     extract (Just (_, v)) = v
                     clean (BenString s) = filter (/= '"') s

getInfoHash :: MetaInfo -> B.ByteString
getInfoHash m = hash . B8.pack $ encoded
            where info = getInfo m
                  encoded = encodeOne $ extract info
                  extract (Just (k,v)) = v

peerIdHash :: String
peerIdHash = B8.unpack . hash . B8.pack $ ['a'..'Z']

port :: String
port = "ABCD"

uploaded :: String
uploaded = show 0

downloaded :: String
downloaded = show 0

toDownload :: MetaInfo -> String
toDownload m = show 0 --get (BenString "length") m - downloaded

-- | 1 for true to allow compact response
compact :: String
compact = show 1

-- | started | stopped | completed
event :: String
event = "started"

urlEncode :: B.ByteString -> String
urlEncode s = concat $ map helper hexs
    where hexs = chunksOf 2 . B8.unpack $ Base16.encode s
          helper hex
               | B8.pack hex `elem` allowed = B8.unpack . first $ (Base16.decode (B8.pack hex))
               | otherwise = '%' : hex           
          first (a,_) = a

allowed = map B8.pack $ chunksOf 2 . B8.unpack $ Base16.encode reserved
        where reserved = B8.pack (['.', '-', '_', '~'] ++ nums ++ letters)
              nums = concat $ map show [0..9]
              letters = ['a'..'z'] ++ ['A'..'Z']

trackerGET :: MetaInfo -> String
trackerGET m = getAnnounceUrl m ++ "?" ++ 
              urlify [("info_hash", urlEncode . getInfoHash $ m),
                      ("peer_id", urlEncode . B8.pack $ peerIdHash),
                      ("port", port),
                      ("uploaded", uploaded),
                      ("downloaded", downloaded),
                      ("left", toDownload m),
                      ("compact", compact),
                      ("event", event)]
              where urlify = intercalate "&" . map paramVal
                    paramVal (p, v) = p ++ "=" ++ v