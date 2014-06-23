import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Crypto.Hash.SHA1 (hash)
import Bencode hiding (main)
import Tracker

import qualified Network.HTTP as HTTP

main :: IO ()
main = do
       args <- getArgs
       fileStr <- B.readFile (args !! 0)
       let meta = parseOne fileStr
           url = trackerGET meta
       body <- HTTP.simpleHTTP (HTTP.getRequest url) >>= HTTP.getResponseBody
       print $ processResponse body