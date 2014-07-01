import           Bencode                (parseOne, MetaInfo, get, BenValue(..))
import           Control.Applicative    ((<$>))
import qualified Data.ByteString        as B
import           Messages               
import           Network                as N        
import           Network.HTTP           (simpleHTTP, getRequest, getResponseBody)
import           Peer                   (Address(..))         
import           Tracker                (announceTracker, getInfoHash)
import           System.Environment

getFilePath :: IO String
getFilePath = do
            args <- getArgs
            case args of  
                 []    -> fail "No file given"
                 (x:_) -> return x

validMeta :: MetaInfo -> Bool
validMeta m = has info announce
          where info = get (BenString "info") m
                announce = get (BenString "announce") m
                has Nothing _ = False
                has _ Nothing = False
                has _ _ = True


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
                  let infoHash = getInfoHash meta
                  print peerList