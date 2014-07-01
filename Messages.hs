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
import           Data.Word              (Word8, Word32)
import           Tracker                (getInfoHash, peerIdHash)
import           Test.HUnit             ((~?=), test, runTestTT)

data HandShake = HandShake { protocol :: String
                           , infoHash :: ByteString
                           , peerId   :: ByteString
                           }
               deriving (Show, Eq)

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have             Integer
             | BitField         BitField
             | Request          Index Integer Integer
             | Piece            Index Integer ByteString
             | Cancel           Index Integer Integer
             | Port             Integer
             deriving (Show, Eq)

type Index = Integer
type BitField = [Bool]

instance Binary HandShake where
         put (HandShake s i p) = do
                               let len = length s
                               putWord8 . fromIntegral $ len
                               putByteString . B8.pack $ s
                               putByteString . B8.pack $ reserved
                               putByteString i
                               putByteString p
                               where reserved = "00000000"
         get = do
             len <- fromIntegral <$> getWord8
             liftA3 HandShake (getString len) 
                              (getByteString 8 >> getByteString 20) 
                              (getByteString 20)
            

--- Not sure what the fail would do while program running for real ..
instance Binary Message where
         put KeepAlive       = putW32 0
         put Choke           = putW32 1 >> putWord8 0
         put Unchoke         = putW32 1 >> putWord8 1 
         put Interested      = putW32 1 >> putWord8 2
         put NotInterested   = putW32 1 >> putWord8 3
         put (Have n)        = putW32 5 >> putWord8 4 >> putW32 n
         put (BitField bf)   = do
                             if length bf `mod` 8 /= 0
                                then fail "BitField length mus b multiple of 8"
                                else do 
                                     let len = 1 + (length bf `div` 8) 
                                     putW32 . fromIntegral $ len
                                     putWord8 5
                                     putBitField bf

         put (Request i b l) = putW32 13 >> putWord8 6 >>
                               putW32 i >> putW32 b >> putW32 l
         put (Piece i b bs)  = do
                             let len = 9 + B.length bs
                             putW32 $ fromIntegral len
                             putWord8 7
                             putW32 i
                             putW32 b
                             putByteString bs
                          
         put (Cancel i b l)  = putW32 13 >> putWord8 8 >>
                               putW32 i >> putW32 b >> putW32 l
         put (Port n)        = putW32 3 >> putWord8 9 >> putW16 n

         get = do
             len <- fromIntegral <$> getWord32be
             if len == 0 
                then return KeepAlive
                else do
                  id <- fromIntegral <$> getWord8
                  case id of
                       0 -> return Choke
                       1 -> return Unchoke
                       2 -> return Interested
                       3 -> return NotInterested
                       4 -> Have <$> getW32
                       5 -> BitField . getBitField <$> getByteString (len - 1)
                       6 -> liftA3 Request getW32 getW32 getW32
                       7 -> liftA3 Piece getW32 getW32 $ getByteString (len - 9)
                       8 -> liftA3 Cancel getW32 getW32 getW32
                       9 -> Port . fromIntegral <$> getWord16be
       
putW16 = putWord16be . fromIntegral
putW32 = putWord32be . fromIntegral 
getW32 = fromIntegral <$> getWord32be
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

-- ............  Tests ...............
putGetTests = test [ f have    ~?= have
                   , f bf      ~?= bf
                   , f req     ~?= req
                   , f cancel  ~?= cancel
                   , f port    ~?= port
                   , f2 hshake ~?= hshake
                   ]
            where f      = runGet get . runPut . put
                  f2     = runGet get . runPut . put
                  have   = Have 12
                  bf     = BitField $ (take 6 $ repeat True) ++ [False, True]
                  req    = Request 0 0 10
                  cancel = Cancel 0 0 10
                  port   = Port 5881
                  hshake = HandShake "BitTorrent Protocol" peerIdHash peerIdHash

blockSize :: Integer
blockSize = 16384