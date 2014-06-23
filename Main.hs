{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Crypto.Hash.SHA1 (hash)
import Bencode hiding (main)
import Tracker

import qualified Network.HTTP.Base as H

main :: IO ()
main = do
       args <- getArgs
       fileStr <- B.readFile (args !! 0)
       let meta = parseMeta fileStr
           ihash = getInfoHash meta
       print . urlEncode $ ihash -- . H.urlEncode . B8.unpack . hash $ B8.pack encd