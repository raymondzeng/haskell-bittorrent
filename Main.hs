import           System.Environment
import qualified Data.ByteString        as B
import           Bencode                (parseOne)
import           Tracker                (requestUrl, processResponse)
import           Wire                   (handShake)
import           Network.HTTP           (simpleHTTP, getRequest, getResponseBody)

main :: IO ()
main = do
       args <- getArgs
       fileStr <- B.readFile (args !! 0)
       let meta = parseOne fileStr
           url = requestUrl meta
           hs = handShake meta
       resp <- simpleHTTP (getRequest url) >>= getResponseBody
       print $ (processResponse resp)