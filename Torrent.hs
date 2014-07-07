module Torrent 
    ( Torrent(..)
    , newTorrent
    , nextRequest
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar 
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.List.Split (chunksOf)
import Bencode
import Messages (Message(..), BitField)
import Tracker (getInfo, getInfoHash)

data Torrent = Torrent
    { infoHash    :: ByteString        -- infohash for torrent
    , myId        :: ByteString        -- peer Id for handshake
    , blockSize   :: Int               -- max size of block for each request
                                          -- default is 16384
    , pieceLength :: Int               -- length of each piece
    , numPieces   :: Int               -- total pieces in torrent
    , haves       :: BitField          -- bitfield of pieces alrady obtained
    , nextReq     :: Maybe (Int, Int)  -- the idx and offset of next block 
    , hashes      :: [ByteString]      -- hashes for each piece
    } deriving (Show)
      
newTorrent :: MetaInfo -> ByteString -> Torrent
newTorrent meta pid = Torrent 
    { infoHash    = getInfoHash meta
    , myId        = pid
    , blockSize   = defaultBSize
    , pieceLength = fromIntegral pieceLen
    , numPieces   = count
    , haves       = take count $ repeat False
    , nextReq     = Just (0,0)
    , hashes      = map B8.pack $ chunksOf 20 pieces
    }
  where Just (_, BenString pieces) = getPieces meta
        Just (_, BenInt pieceLen) = getPieceLen meta
        count = length pieces `div` 20
        defaultBSize = 16384

-- returns Nothing if no more pieces to get
nextToReq :: TVar Torrent 
          -> STM (Maybe (Int, Int)) -- the (idx, offset) for the request message
nextToReq tTor = do
    tor <- readTVar tTor
    case nextReq tor of
      Nothing -> return Nothing
      Just (idx, offset) -> do
          if (pieceLength tor) - offset <= blockSize tor
            then if idx >= numPieces tor 
                   then makeNext Nothing
                   else makeNext $ Just (idx + 1, 0)
            else makeNext $ Just (idx, offset + blockSize tor)
          return $ Just (idx, offset)
  where makeNext :: Maybe (Int, Int) -> STM ()
        makeNext n = modifyTVar tTor (\t -> t {nextReq = n})

-- piece length and blocksize never change. so if possible to make only certain fields of Torrent TVar..
nextRequest :: TVar Torrent -> STM (Maybe Message)
nextRequest tTor = do
    req <- nextToReq tTor
    case req of
      Nothing -> return Nothing
      Just (idx, offset) -> do
          remainder <- (flip (-) offset) . pieceLength <$> readTVar tTor
          bs <- blockSize <$> readTVar tTor
          let len = min remainder bs
          return . Just $ Request (fromIntegral idx) -- convert Int to Word32
                                  (fromIntegral offset) 
                                  (fromIntegral len)

-- ...... Utils
getPieces :: MetaInfo -> Maybe (BenValue, BenValue)
getPieces m = getFromDict (BenString "pieces") $ val (getInfo m)
  where val (Just (_,v)) = v

getPieceLen :: MetaInfo -> Maybe (BenValue, BenValue)
getPieceLen m = getFromDict (BenString "piece length") $ val (getInfo m)
  where val (Just (_,v)) = v