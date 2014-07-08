module Peer where

import           Control.Applicative         ((<$>), (<*>))
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.STM      (STM)
import           Control.Concurrent.STM.TVar ( TVar
                                             , readTVar
                                             , readTVarIO
                                             , writeTVar
                                             , newTVar
                                             )
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
                                             , Block(..)
                                             , HandShake(..)
                                             , Message(..)
                                             )
import           Torrent
import           Tracker                     (Address)

data Peer = Peer 
    { getHandle      :: Handle
    , maybeId        :: Maybe String 
    , amInterested   :: TVar Bool
    , amChoking      :: TVar Bool
    , theyInterested :: TVar Bool
    , theyChoking    :: TVar Bool
    , bitfield       :: TVar BitField
    , reqPending     :: TVar Bool
    } 

newPeer :: Handle -> STM Peer
newPeer handle = do
    ai <- newTVar False
    ac <- newTVar True
    ti <- newTVar False
    tc <- newTVar True
    bf <- newTVar []
    rp <- newTVar False
    return Peer 
        { getHandle      = handle 
        , maybeId        = Nothing
        , amInterested   = ai
        , amChoking      = ac
        , theyInterested = ti
        , theyChoking    = tc
        , bitfield       = bf
        , reqPending     = rp
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
listenToPeer :: Torrent -> Peer -> IO ()
listenToPeer tor peer = forever $ do
    msg <- getMessage $ getHandle peer
    case msg of
        Choke         -> atomically (writeTVar (theyChoking peer) True)
                         >> print "choke"
        Unchoke       -> atomically (writeTVar (theyChoking peer) False)
                         >> print "unchoke"
        Interested    -> atomically (writeTVar (theyInterested peer) True)
                         >> print "interested"
        NotInterested -> atomically (writeTVar (theyInterested peer) False)
                         >> print "notinterested"
        Have n        -> do
               new <- atomically $ updatedBf n peer
               atomically (writeTVar (bitfield peer) new)
               >> print ("Have " ++ show n)
        BitField bf   -> atomically (writeTVar (bitfield peer) bf)
                         >> print "BitField"
        Piece b@(Block i o s) -> do
               atomically $ writeTVar (reqPending peer) False
               valid <- consumeBlock tor b
               case valid of
                 Nothing -> return ()
                 Just True -> print "Piece Hash is VALID"
                 Just False -> print "Piece Hash BAD"
               print $ "Piece " ++ show i ++ " " ++ show o
        _             -> print "unknown message"
              

getMessage :: Handle -> IO Message
getMessage handle = do
    bytes <- Lazy.hGet handle 4
    let mlen = toInt bytes
    msg <- Lazy.hGet handle mlen
    return $ Bin.decode (bytes <> msg)

-- returns a new bitfield with the element at index n of the peer's bitfield
-- changed to True
updatedBf :: Int -> Peer -> STM BitField
updatedBf n peer = do
    old <- readTVar $ bitfield peer
    return $ updatePieces n old

-- ...... The stuff that sends requests
talkToPeer :: Torrent -> Peer -> IO ()
talkToPeer tor peer = whileM_ (notComplete) (makeRequests tor peer)
                      >> print "Torrent completely downloaded"
  where notComplete = not <$> readTVarIO (done tor)

makeRequests :: Torrent -> Peer -> IO ()
makeRequests tor peer = do    
    tc <- readTVarIO $ theyChoking peer
    if tc
      then do 
        sendMessage Interested peer
        threadDelay 1000000
      else do
        rp <- readTVarIO (reqPending peer)
        if rp 
           then threadDelay 500000
           else do 
             maybeReq <- atomically . nextRequest $ tor
             case maybeReq of
               Nothing -> return ()
               Just req -> do
                   print req
                   sendMessage req peer
                   atomically $ writeTVar (reqPending peer) True

sendMessage :: Message -> Peer -> IO ()
sendMessage msg peer = do
    Lazy.hPut handle (Bin.encode msg)
   -- print $ "sent " ++ show msg
  where handle = getHandle peer

-- ...... Utilities
toInt :: Lazy.ByteString -> Int
toInt bs = fromIntegral (Bin.decode $ bs :: Word32)

-- | Execute an action repeatedly as long as the given boolean expression
-- returns True.  The condition is evaluated before the loop body.
-- Discards results.
-- Taken from monad-loops
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            if x
                then f >> go
                else return ()