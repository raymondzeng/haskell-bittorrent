module Messages where

import           Bencode                (MetaInfo)
import           Control.Applicative    (liftA3, (<$>), (<*>), (*>))
import           Data.Binary            (Binary, get, put)
import           Data.Binary.Put
import           Data.Binary.Get
import           Data.Bits.Bitwise      (packWord8BE, unpackWord8BE)
import           Data.Bits              (testBit)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B 
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as BL
import           Data.List.Split        (chunksOf)
import           Data.Word              (Word8, Word16, Word32, Word64)
import           Tracker                (getInfoHash, peerIdHash)
import           Test.HUnit             ((~?=), test, runTestTT)


data HandShake = HandShake { protocol :: String
                           , reserved :: Word64
                           , infoHash :: ByteString
                           , peerId   :: ByteString
                           }
                           deriving (Show, Eq)

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have             Word32
             | BitField         BitField
             | Request          Index Word32 Word32
             | Piece            Index Word32 ByteString
             | Cancel           Index Word32 Word32
             | Port             Word16
             | Extended         
             deriving (Show, Eq)

type Index = Word32
type BitField = [Bool]

instance Binary HandShake where
    put (HandShake s r i p) = do
                              let len = length s
                              putWord8 . fromIntegral $ len
                              putByteString . B8.pack $ s
                              putWord64be r
                              putByteString i
                              putByteString p
    get = do
          len <- fromIntegral <$> getWord8
          HandShake <$> (getString len) 
                        <*> (getWord64be) 
                        <*> (getByteString 20) 
                        <*> (getByteString 20)
            

--- Not sure what the fail would do while program running for real ..
instance Binary Message where
    put KeepAlive       = putWord32be 0
    put Choke           = putWord32be 1 >> putWord8 0
    put Unchoke         = putWord32be 1 >> putWord8 1 
    put Interested      = putWord32be 1 >> putWord8 2
    put NotInterested   = putWord32be 1 >> putWord8 3
    put (Have n)        = putWord32be 5 >> putWord8 4 >> putWord32be n
    put (BitField bf)   = do
                          if length bf `mod` 8 /= 0
                             then fail "BitField length mus b multiple of 8"
                             else do 
                                  let len = 1 + (length bf `div` 8) 
                                  putWord32be . fromIntegral $ len
                                  putWord8 5
                                  putBitField bf

    put (Request i b l) = putWord32be 13 >> putWord8 6 >>
                          putWord32be i >> putWord32be b >> putWord32be l
    put (Piece i b bs)  = do
                          let len = 9 + B.length bs
                          putWord32be $ fromIntegral len
                          putWord8 7
                          putWord32be i
                          putWord32be b
                          putByteString bs
                          
    put (Cancel i b l)  = putWord32be 13 >> putWord8 8 >>
                          putWord32be i >> putWord32be b >> putWord32be l
    put (Port n)        = putWord32be 3 >> putWord8 9 >> putWord16be n
    put Extended        = putWord32be 1 >> putWord8 20

    get = do
        len <- fromIntegral <$> getWord32be
        if len == 0 
           then return KeepAlive
           else matchId len

matchId :: Int -> Get Message
matchId len = do
    id <- fromIntegral <$> getWord8
    case id of
         0  -> return Choke
         1  -> return Unchoke
         2  -> return Interested
         3  -> return NotInterested
         4  -> Have <$> getWord32be
         5  -> BitField . getBitField <$> getByteString (len - 1) 
         6  -> liftA3 Request getWord32be getWord32be getWord32be
         7  -> liftA3 Piece getWord32be getWord32be $ getByteString (len - 9) 
         8  -> liftA3 Cancel getWord32be getWord32be getWord32be 
         9  -> Port . fromIntegral <$> getWord16be
         20 -> return Extended
         _  -> fail $ "Failed match: length " ++ show len ++ "; id: " ++ show id

getString :: Int -> Get String
getString n = B8.unpack <$> getByteString n

-- ??? packWord8BE takes 8 booleans as arguments
-- is there a better way to do this
putBitField :: [Bool] -> Put
putBitField bf = putByteString . B.pack $ map (apply8 packWord8BE) tuples
    where tuples = chunksOf 8 bf
          apply8 fun (a:b:c:d:e:f:g:h:[]) = fun a b c d e f g h
          apply8 fun l = apply8 fun (l ++ padding)
                 where missing = 8 - length l 
                       padding = take missing $ repeat False

-- testBit considers the 0th bit as the one at tail of the list so
-- we use [7,6..0] as opposed to [0..7]
getBitField :: ByteString -> [Bool]
getBitField bs = concat $ map (\byte -> map (testBit byte) [7,6..0]) bytes
    where bytes = B.unpack bs

blockSize :: Integer
blockSize = 16384
