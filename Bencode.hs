import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Applicative ((<$>), (<*>), (<|>), (<*), (*>))
import System.Environment
import Test.HUnit
import Data.List (find)

data BenValue = ByteString B.ByteString
              | Integer Integer
              | List [BenValue]
              | Dict [(Key, BenValue)]
              deriving (Eq, Show)

type Key = BenValue -- keys for dictionaries must be strings

type Metadata = [BenValue]

-- | Parses an int delimited by i and e, i<int>e
-- | "i35e" would be parsed as 35.
-- | Can handle signed ints (leading +/-) so "i-20e" will be parsed as -20
parseInt :: P.Parser BenValue
parseInt = Integer <$> (P.char 'i' *> P.signed P.decimal <* P.char 'e')

-- | Parses a string preceeded by its length and a colon, <#>:<string>
-- | "4:abcd" would be parsed as "abcd" and "4:hello" as "hell"
parseString :: P.Parser BenValue
parseString = do
            n <- P.decimal -- length of string
            P.char ':'
            s <- P.take n 
            return . ByteString $ s            

parseList :: P.Parser BenValue
parseList = List <$> (P.char 'l' *> P.many' parseExpr <* P.char 'e')

-- | Parses a dictionary delimited by d and e, d<key_value pairs>e
-- | kv pairs are not seperated by anything
-- | all keys are BenValue Strings
-- | values can be any BenValue
parseDict :: P.Parser BenValue
parseDict = Dict <$> (P.char 'd' *> P.many' pDictEntry <* P.char 'e')

-- | Parses a single dict entry, or key-value pair
pDictEntry :: P.Parser (Key, BenValue)
pDictEntry = (,) <$> parseString <*> parseExpr
      
-- | Parses any one of the possible BenValues
parseExpr :: P.Parser BenValue
parseExpr = parseInt
          <|> parseString
          <|> parseList          
          <|> parseDict

-- | Parses an entire string (of metadata) and produces a [BenValue]
parseMeta :: P.Parser Metadata
parseMeta = P.manyTill (parseExpr) (P.endOfInput)

main :: IO ()
main = do
       args <- getArgs
       B.readFile (args !! 0) >>= print . P.parseOnly parseMeta
       
     --  B.readFile (args !! 0) >>= getM . P.parseOnly parseMeta
     --  where getM (Right m) = B.putStr $ B8.pack $ show m

-- | takes a key and a dict and returns the associated benvalue
get :: Key -> BenValue -> Maybe (Key, BenValue)
get k (Dict kvs) = getHelper k kvs
get _ _ = Nothing

getHelper :: Key -> [(Key, BenValue)] -> Maybe (Key, BenValue)
getHelper k kvs = find (\(s,v) -> s == k) kvs

-- | Tests

parseOne :: String -> BenValue
parseOne s = f $ P.parseOnly parseExpr (B8.pack s)
    where f (Right v) = v

-- | fetches the "pieces" string from the metainfo
getPieces m = get pieces $ val (getInfo m)
         where pieces = ByteString (B8.pack "pieces")
               val (Just (_,v)) = v

getInfo m = get info m
        where info = ByteString (B8.pack "info")

pieceString m = f $ getPieces m 
            where f (Just (_,v)) = v

parseTests = test [ -- parseInt
                   parseOne "i2e" ~?= (Integer 2),
                   parseOne "i45e" ~?= (Integer 45),
                   parseOne "i456456345623454654356354e" ~?= (Integer 456456345623454654356354),
                   parseOne "i-9e" ~?= (Integer (-9)),
                   parseOne "i-23e" ~?= (Integer (-23)),
                   parseOne "i+34e" ~?= (Integer 34),
                   -- parseString
                   parseOne "0:" ~?= (ByteString (B8.pack "")),
                   parseOne "1:a" ~?= (ByteString (B8.pack "a")),
                   parseOne "1:1" ~?= (ByteString (B8.pack "1")),
                   parseOne "5:hello" ~?= (ByteString (B8.pack "hello")),
                   parseOne "5:ab34c" ~?= (ByteString (B8.pack "ab34c")),
                   parseOne "5:-$@#!" ~?= (ByteString (B8.pack "-$@#!")),
                   parseOne "3:3:1" ~?= (ByteString (B8.pack "3:1")),
                   parseOne "1:xdiscarded" ~?= (ByteString (B8.pack "x")),
                   -- parse List 
                   parseOne "le" ~?= (List []),
                   parseOne "llee" ~?= (List [List []]),
                   parseOne "llleee" ~?= (List [List [List []]]),
                   parseOne "li2e3:abce" ~?= (List [Integer 2, ByteString (B8.pack "abc")]), 
                   parseOne "lli2eee" ~?= (List [List [Integer 2]]),
                   parseOne "lli2eeei2e5:disca" ~?= (List [List [Integer 2]]),
                   -- parse dict
                   parseOne "de" ~?= (Dict []),
                   parseOne "d1:a1:x1:b1:ye" ~?= (Dict [(ByteString (B8.pack "a"), ByteString (B8.pack "x")), 
                                                        (ByteString (B8.pack "b"), ByteString (B8.pack "y"))])]