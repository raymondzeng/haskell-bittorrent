module Wire where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B 
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as BL
import           Data.Word              (Word8, Word32)
import           Data.Binary            (Binary, get, put)
import           Data.Binary.Put
import           Data.Binary.Get       
import           Bencode                (MetaInfo)
import           Tracker                (getInfoHash, peerIdHash)
import           Control.Applicative    (liftA3, (<$>), (<*>), (*>))
import           Test.HUnit             ((~?=), test, runTestTT)

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have Integer
             | BitField ByteString
             | Request Index Integer Integer
             | Piece Index Integer ByteString
             | Cancel Index Integer Integer
             | Port Integer
             deriving (Show, Eq)

type Index = Integer

instance Binary Message where
         put KeepAlive       = putW32 0
         put Choke           = putW32 1 >> putWord8 0
         put Unchoke         = putW32 1 >> putWord8 1 
         put Interested      = putW32 1 >> putWord8 2
         put NotInterested   = putW32 1 >> putWord8 3
         put (Have n)        = putW32 5 >> putWord8 4 >> putW32 n
         put (BitField bf)   = do
                             let len = 1 + B.length bf 
                             putW32 . fromIntegral $ len
                             putWord8 5
                             putByteString bf

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
                       5 -> BitField <$> getByteString (len - 1)
                       6 -> liftA3 Request getW32 getW32 getW32
                       7 -> liftA3 Piece getW32 getW32 $ getByteString (len - 9)
                       8 -> liftA3 Cancel getW32 getW32 getW32
                       9 -> Port . fromIntegral <$> getWord16be
       
putW16 = putWord16be . fromIntegral
putW32 = putWord32be . fromIntegral 
getW32 = fromIntegral <$> getWord32be

handShake :: MetaInfo -> B.ByteString
handShake m = B.concat [pstrlen, pstr, reserved, infoHash, peerIdHash]
          where pstrlen = B.singleton (19 :: Word8)
                pstr = B8.pack "BitTorrent protocol"
                reserved = B8.pack "00000000"
                infoHash = getInfoHash m


putGetTests = test [ f have   ~?= have,
                     f bf     ~?= bf,
                     f req    ~?= req,
                     f cancel ~?= cancel,
                     f port   ~?= port ]
            where f      = runGet get . runPut . put
                  have   = Have 12
                  bf     = BitField $ B8.pack "100100"
                  req    = Request 0 0 10
                  cancel = Cancel 0 0 10
                  port   = Port 5881
               