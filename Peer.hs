module Peer where

import           Control.Applicative         ((<*>))
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.STM.TVar ( TVar
                                             , readTVarIO
                                             , modifyTVar)
import           Control.Monad               (forever)
import           Control.Monad.STM           (atomically)
import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Bin 
import           Data.Binary.Put             (runPut)
import           Data.Binary.Get             (runGet)
import qualified Data.ByteString.Lazy        as Lazy
import           Data.List                   (findIndex)
import           Data.Monoid                 ((<>))
import           Data.Word                   (Word8, Word16, Word32)
import           System.IO                   (Handle)

import           Messages                    ( BitField 
                                             , HandShake(..)
                                             , Message(..))
import           Tracker                     (Address)

data Peer = Peer 
    { getHandle      :: Handle
    , maybeId        :: Maybe String 
    , amInterested   :: Bool
    , amChoking      :: Bool
    , theyInterested :: Bool
    , theyChoking    :: Bool
    , bitfield       :: BitField
    , reqPending     :: Bool
    } deriving (Show)

newPeer :: Handle -> Peer
newPeer handle = Peer 
                 { getHandle      = handle 
                 , maybeId        = Nothing
                 , amInterested   = False
                 , amChoking      = True
                 , theyInterested = False
                 , theyChoking    = True
                 , bitfield       = []
                 , reqPending     = False
                 }

-- ... ....... Handshake stuff
sendHandShake :: HandShake -> Peer -> IO ()
sendHandShake hs peer = Lazy.hPut handle (Bin.encode hs)
    where handle = getHandle peer
                   
getHandShake :: Peer -> IO HandShake
getHandShake peer = do
    let handle = getHandle peer
    byte <- Lazy.hGet handle 1
    let mlen = fromIntegral $ Lazy.head byte
    msg <- Lazy.hGet handle (mlen + 48)
    return $ Bin.decode (byte <> msg)
                  
-- TODO : verify peer id
validateHandShake :: HandShake -> HandShake -> Either String ()
validateHandShake to@(HandShake _ _ tih _) from@(HandShake fport _ fih _)
    | fport /= "BitTorrent protocol" = Left ("Peer sent HS with bad protocol: " ++ fport)
    | fih /= tih = Left "Peer's HS info hash does not match"
    | otherwise = Right ()

-- ......... The stuff that listens and changes state
-- ......... listenToPeer should be run concurr with requestStuff

listenToPeer :: TVar Peer -> TVar BitField -> IO ()
listenToPeer tvPeer gTvBf = forever $ do
    peer <- readTVarIO tvPeer
    msg <- getMessage $ getHandle peer
    case msg of
        Choke         -> updatePeer tvPeer (\p -> p {theyChoking = True})
                         >> print "choke"
        Unchoke       -> updatePeer tvPeer (\p -> p {theyChoking = False}) 
                         >> print "unchoke"
        Interested    -> updatePeer tvPeer (\p -> p {theyInterested = True})
        NotInterested -> updatePeer tvPeer (\p -> p {theyInterested = False})
        Have n        -> updatePeer tvPeer (updateBF n)
                         >> print ("Have " ++ show n)
        BitField bf   -> updatePeer tvPeer (\p -> p {bitfield = bf}) 
                         >> print "BitField"
        Piece i _ _   -> do
            print i
            atomically $ modifyTVar gTvBf (updatePieces i)
            readTVarIO gTvBf >>= print
       --     updatePeer tvPeer (\p -> p {reqPending = False})
        _             -> do
            p <- readTVarIO tvPeer
            print p

getMessage :: Handle -> IO Message
getMessage handle = do
    bytes <- Lazy.hGet handle 4
    let mlen = toInt bytes
    msg <- Lazy.hGet handle mlen
    return $ Bin.decode (bytes <> msg)

updatePeer :: TVar Peer -> (Peer -> Peer) -> IO ()
updatePeer peer withF = atomically $ modifyTVar peer withF

updateList :: Int -> a -> [a] -> [a]
updateList n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x : (updateList (n-1) newVal xs)
     
updateBF :: Word32 -> Peer -> Peer
updateBF n peer = peer {bitfield = newBF}
  where newBF = updateList (fromIntegral n) True (bitfield peer)

updatePieces :: Word32 -> BitField -> BitField
updatePieces n bf = updateList (fromIntegral n) True bf
             
-- ...... The stuff that sends requests
requestStuff :: TVar Peer -> TVar BitField -> IO ()
requestStuff tvPeer gTvBf = forever $ do
    peer <- readTVarIO tvPeer
    gBf <- readTVarIO gTvBf
    case findIndex (==False) gBf of
        Nothing  -> print "Got all pieces"
        Just idx -> do
            let req = Request (fromIntegral idx) 0 blockSize
            sendMessage req peer
            threadDelay 500000

sendMessage :: Message -> Peer -> IO ()
sendMessage msg peer = Lazy.hPut handle (Bin.encode msg)
  where handle = getHandle peer


-- ...... Utilities
toInt :: Lazy.ByteString -> Int
toInt bs = fromIntegral (Bin.decode $ bs :: Word32)


blockSize :: Word32
blockSize = 16384