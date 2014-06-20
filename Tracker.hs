module Tracker where

import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Crypto.Hash.SHA1 (hash)
import Bencode hiding (main)
import Data.Char

-- Takes a bytestring and encodes it according to bittorrent tracker protocol standard
-- reserved characters : 0-9, a-z, A-Z, '.', '-', '_' and '~'
urlencode bs = B8.filter (not . (flip elem) reserved) bs
          where 
                reserved = ".-_~" ++ ['a'..'z'] ++ ['A'..'Z'] 
                         ++ "0123456789"


main :: IO ()
main = do
       args <- getArgs
       fileStr <- B.readFile (args !! 0)
       let dict = (parseAll fileStr) !! 0
           info = getInfo dict
           encd = encodeOne $ ex info
           ex (Just (k,v)) = v
       print $ urlencode (hash encd)