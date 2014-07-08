module Torrent 
    ( Torrent(..)
    , Block(..)
    , newTorrent
    , nextRequest
    , consumeBlock
    , updatePieces
    ) where

import           Control.Applicative         ((<$>))
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar 
import           Crypto.Hash.SHA1            (hash)
import qualified Data.ByteString             as B
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as B8
import           Data.List                   (sort)
import           Data.List.Split             (chunksOf)

import           Bencode
import           Messages                    ( Message(..), 
                                             , BitField
                                             , Block(..)
                                             )
import           Tracker                     ( getInfo
                                             , getInfoHash
                                             )

data Torrent = Torrent
    { fileName    :: String            -- name of the file to write the data to
    , infoHash    :: ByteString        -- infohash for torrent
    , myId        :: ByteString        -- peer Id for handshake
    , blockSize   :: Int               -- max size of block for each request
    , pieceLength :: Int               -- length of each piece
    , blocksPer   :: Int               -- blocks per piece
    , numPieces   :: Int               -- total pieces in torrent
    , haves       :: TVar BitField     -- bitfield of pieces alrady obtained
    , nextReq     :: TVar (Maybe (Int, Int)) -- the idx and offset of next block
    , hashes      :: [ByteString]      -- hashes for each piece
    , pieceBuffer :: TVar [[Block]]    
    , lastWritten :: TVar Int
    , done        :: TVar Bool
    } 
      
newTorrent :: MetaInfo -> ByteString -> STM Torrent
newTorrent meta pid = do
    tHaves <- newTVar $ take count $ repeat False
    tNext <- newTVar $ Just (0,0)
    tBuff <- newTVar $ take count $ repeat []
    tLast <- newTVar $ 0
    tDone <- newTVar $ False
    return Torrent 
        { fileName    = fName
        , infoHash    = getInfoHash meta
        , myId        = pid
        , blockSize   = defaultBSize
        , pieceLength = pieceLen
        , blocksPer   = bp
        , numPieces   = count
        , haves       = tHaves
        , nextReq     = tNext
        , hashes      = map B8.pack $ chunksOf 20 pieces
        , pieceBuffer = tBuff
        , lastWritten = tLast
        , done        = tDone
        }
  where Just (_, BenString pieces) = getPieces meta
        Just (_, BenInt pieceLen) = getPieceLen meta
        Just (_, BenString fName) = getFileName meta
        count = length pieces `div` 20
        defaultBSize = 16384
        bp = ceiling $ (realToFrac pieceLen) / (realToFrac defaultBSize)

-- if there are no more pieces to request,
-- returns Nothing and sets done in tor to True
-- otherwise returns Just (idx, offset)
nextToReq :: Torrent 
          -> STM (Maybe (Int, Int)) -- the (idx, offset) for the request message
nextToReq tor = do
    next <- readTVar (nextReq tor)
    case next of
      Nothing -> return Nothing
      Just (idx, offset) -> do
          if (pieceLength tor) - offset <= blockSize tor
            then if idx >= numPieces tor - 2
                   then do
                     writeTVar (done tor) True
                     makeNext Nothing
                   else makeNext $ Just (idx + 1, 0)
            else makeNext $ Just (idx, offset + blockSize tor)
          return $ Just (idx, offset)
  where makeNext :: Maybe (Int, Int) -> STM ()
        makeNext n = writeTVar (nextReq tor) n

-- TODO : currently just increments. should take into account haves
nextRequest :: Torrent -> STM (Maybe Message)
nextRequest tor = do
    maybeReq <- nextToReq tor
    case maybeReq of
      Nothing -> return Nothing
      Just (idx, offset) -> do
          let len = min (blockSize tor) (pieceLength tor - offset)
          return . Just $ Request idx offset len

-- TODO : drop connection if invalid hash. needs to boil up exception

-- returns Nothing if this block doesn't complete a piece
-- otherwise, hashes the piece formed by the blocks and returns
-- whether that hash matches the hash in the .torrent metainfo
consumeBlock :: Torrent -> Block -> IO (Maybe Bool)
consumeBlock tor b@(Block i o c) = do
    atomically $ modifyTVar (haves tor) (updatePieces i)
    atomically $ modifyTVar (pieceBuffer tor) (updateBuffer b)
    blockBuff <- (!! i) <$> readTVarIO (pieceBuffer tor)
    if length blockBuff == (blocksPer tor)
       then do
           let valid = checkHash tor i blockBuff
           if valid 
              then writeOut tor i 
              else return () -- TODO
           return $ Just valid
       else return Nothing

checkHash :: Torrent -> Int -> [Block] -> Bool
checkHash tor i blocks = realHash == toCheck
  where bstrings = map (\(Block _ _ c) -> c) (sort blocks)
        piece = B.concat bstrings
        toCheck = hash piece
        realHash = (hashes tor) !! i

-- TODO : check if file with name already exists before appending
writeOut :: Torrent -> Int -> IO ()
writeOut tor i = do
    blocks <- (!! i) <$> readTVarIO (pieceBuffer tor)
    let piece = B.concat $ map (\(Block _ _ c) -> c) (sort blocks)
    B.appendFile (fileName tor) piece

-- ...... Utils
getFileName :: MetaInfo -> Maybe (BenValue, BenValue)
getFileName m = getFromDict (BenString "name") $ val (getInfo m)
  where val (Just (_,v)) = v

getPieces :: MetaInfo -> Maybe (BenValue, BenValue)
getPieces m = getFromDict (BenString "pieces") $ val (getInfo m)
  where val (Just (_,v)) = v

getPieceLen :: MetaInfo -> Maybe (BenValue, BenValue)
getPieceLen m = getFromDict (BenString "piece length") $ val (getInfo m)
  where val (Just (_,v)) = v

-- returns a new list with the element at index n changed to newVal
updateList :: Int -> a -> [a] -> [a]
updateList n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x : (updateList (n-1) newVal xs)

-- returns a new bitfiled with elem at idx n changed to True
updatePieces :: Int -> BitField -> BitField
updatePieces n bf = updateList n True bf

-- Takes a Block and updates the Block buffer at indx i in the Piece Buffer
updateBuffer :: Block -> [[Block]] -> [[Block]]
updateBuffer b@(Block i o c) pBuff  = updateList i bBuff pBuff
  where bBuff = b : pBuff !! i 