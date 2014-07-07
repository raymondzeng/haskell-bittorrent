module Messages where

import           Control.Applicative    (liftA2, liftA3, (<$>), (<*>))
import           Data.Binary            (Binary, get, put)
import           Data.Binary.Put        
import           Data.Binary.Get
import           Data.Bits.Bitwise      (packWord8BE)
import           Data.Bits              (testBit)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B 
import qualified Data.ByteString.Char8  as B8
import           Data.List.Split        (chunksOf)
import           Data.Word              (Word16, Word32, Word64)

data Block = Block 
    { index   :: Int
    , offset  :: Int
    , content :: ByteString
    } deriving (Show, Eq)

instance Ord Block where
    compare (Block i1 o1 _) (Block i2 o2 _) 
        | i1 /= i2  = compare i1 i2
        | otherwise = compare o1 o2

data HandShake = HandShake 
    { protocol :: String
    , reserved :: Word64
    , infoHash :: ByteString
    , peerId   :: ByteString
    } deriving (Show, Eq)

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have             Int
             | BitField         BitField
             | Request          Int Int Int
             | Piece            Block
             | Cancel           Int Int Int
             | Port             Word16
             | Extended         
             deriving (Show, Eq)

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
    put (Have n)        = putWord32be 5 >> putWord8 4 >> put32 n
    put (BitField bf)   = do
                          if length bf `mod` 8 /= 0
                             then fail "BitField length must b multiple of 8"
                             else do 
                                  let len = 1 + (length bf `div` 8) 
                                  put32 len
                                  putWord8 5
                                  putBitField bf

    put (Request i b l) = putWord32be 13 >> putWord8 6 >>
                          put32 i >> put32 b >> put32 l
    put (Piece (Block i b bs))  = do
                          let len = 9 + B.length bs
                          put32 len
                          putWord8 7
                          put32 i
                          put32 b
                          putByteString bs
                          
    put (Cancel i b l)  = putWord32be 13 >> putWord8 8 >>
                          put32 i >> put32 b >> put32 l
    put (Port n)        = putWord32be 3 >> putWord8 9 >> putWord16be n
    put Extended        = putWord32be 1 >> putWord8 20

    get = do
        len <- get32
        case len of
            0         -> return KeepAlive
            _         -> matchId len

matchId :: Int -> Get Message
matchId len = do
    mId <- fromIntegral <$> getWord8
    case mId of
        0  -> return Choke
        1  -> return Unchoke
        2  -> return Interested
        3  -> return NotInterested
        4  -> Have <$> get32
        5  -> BitField . getBitField <$> getByteString (len - 1) 
        6  -> liftA3 Request get32 get32 get32
        7  -> do
               block <- liftA3 (Block) get32 get32 $ getByteString (len - 9) 
               return $ Piece block
        8  -> liftA3 Cancel get32 get32 get32 
        9  -> Port . fromIntegral <$> getWord16be
        20 -> return Extended
        _  -> fail $ "Failed match: length " ++ show len ++ " id: " ++ show mId

put32 :: Int -> Put
put32 = putWord32be . fromIntegral

get32 :: Get Int
get32 = fromIntegral <$> getWord32be

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
