{-# LANGUAGE OverloadedStrings #-}

module Bencode where

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Char8 (pack)
import Control.Applicative ((<$>), (<*>), (<|>), (<*), (*>))
import System.Environment (getArgs)
import Test.HUnit ((~?=), test, runTestTT)
import Data.List (find)


-- TODO : 
-- put a stricter type on the keys of Dict so that they HAVE to be BenString?
-- Error testing, make sure parsing works on bad data

data BenValue = BenString String
              | BenInt Integer
              | BenList [BenValue]
              | BenDict [(Key, BenValue)]
              deriving (Eq, Show)

type Key = BenValue -- keys for dictionaries must be strings

type MetaInfo = BenValue

-- ................... Decoding .................

-- | Parses an int delimited by i and e, i<int>e
-- | "i35e" would be parsed as 35.
-- | Can handle signed ints (leading +/-) so "i-20e" will be parsed as -20
parseInt :: P.Parser BenValue
parseInt = BenInt <$> (P.char 'i' *> P.signed P.decimal <* P.char 'e')

-- | Parses a string preceeded by its length and a colon, <#>:<string>
-- | "4:abcd" would be parsed as "abcd" and "4:hello" as "hell"
parseString :: P.Parser BenValue
parseString = do
            n <- P.decimal -- length of string
            P.char ':'
            s <- P.take n 
            return . BenString $ B8.unpack s            

parseList :: P.Parser BenValue
parseList = BenList <$> (P.char 'l' *> P.many' parseExpr <* P.char 'e')

-- | Parses a dictionary delimited by d and e, d<key_value pairs>e
-- | kv pairs are not seperated by anything
-- | all keys are BenValue Strings
-- | values can be any BenValue
parseDict :: P.Parser BenValue
parseDict = BenDict <$> (P.char 'd' *> P.many' pDictEntry <* P.char 'e')

-- | Parses a single dict entry, or key-value pair
pDictEntry :: P.Parser (Key, BenValue)
pDictEntry = (,) <$> parseString <*> parseExpr
      
-- | Parses any one of the possible BenValues
parseExpr :: P.Parser BenValue
parseExpr = parseInt
          <|> parseString
          <|> parseList          
          <|> parseDict

parseOne :: String -> BenValue
parseOne s = f $ P.parseOnly parseExpr (pack s)
         where f (Right v) = v

parseAll :: B.ByteString -> [BenValue]
parseAll s = extract $ P.parseOnly helper s
         where extract (Right x) = x
               helper = P.manyTill parseExpr P.endOfInput

parseMeta :: B.ByteString -> MetaInfo
parseMeta s = extract $ P.parseOnly parseExpr s
          where extract (Right x) = x

-- ................... Accessors .................
-- | takes a key and a dict and returns the associated benvalue
get :: Key -> BenValue -> Maybe (Key, BenValue)
get k (BenDict kvs) = getHelper k kvs
get _ _ = Nothing

getHelper :: Key -> [(Key, BenValue)] -> Maybe (Key, BenValue)
getHelper k kvs = find (\(s,v) -> s == k) kvs

-- ................... Encoding .................
encodeString :: BenValue -> String
encodeString (BenString s) = show (length s) ++ ":" ++ s

encodeInt :: BenValue -> String
encodeInt (BenInt i) = "i" ++ show i ++ "e"

encodeList :: BenValue -> String
encodeList (BenList l) = "l" ++ foldl1 (++) (map encodeOne l) ++ "e"

-- enforces that keys are strings by only encoding string on them
-- throws non exchaustive pattern match if key is not string
encodeDict :: BenValue -> String
encodeDict (BenDict d) = "d" ++ foldl1 (++) (map dEntryEncode d) ++ "e"
                       where dEntryEncode (k,v) = encodeString k ++ encodeOne v

encodeOne :: BenValue -> String
encodeOne b@(BenString _) = encodeString b
encodeOne b@(BenInt _) = encodeInt b
encodeOne b@(BenList _) = encodeList b
encodeOne b@(BenDict _) = encodeDict b

-- ................... Testing .................

parseTests = test [ -- parseInt
                   parseOne "i2e" ~?= (BenInt 2),
                   parseOne "i45e" ~?= (BenInt 45),
                   parseOne "i456456345623454654356354e" ~?= (BenInt 456456345623454654356354),
                   parseOne "i-9e" ~?= (BenInt (-9)),
                   parseOne "i-23e" ~?= (BenInt (-23)),
                   parseOne "i+34e" ~?= (BenInt 34),
                   -- parseString
                   parseOne "0:" ~?= (BenString ""),
                   parseOne "1:a" ~?= (BenString "a"),
                   parseOne "1:1" ~?= (BenString "1"),
                   parseOne "5:hello" ~?= (BenString "hello"),
                   parseOne "5:ab34c" ~?= (BenString "ab34c"),
                   parseOne "5:-$@#!" ~?= (BenString "-$@#!"),
                   parseOne "3:3:1" ~?= (BenString "3:1"),
                   parseOne "1:xdiscarded" ~?= (BenString "x"),
                   -- parse List 
                   parseOne "le" ~?= (BenList []),
                   parseOne "llee" ~?= (BenList [BenList []]),
                   parseOne "llleee" ~?= (BenList [BenList [BenList []]]),
                   parseOne "li2e3:abce" ~?= (BenList [BenInt 2, BenString "abc"]), 
                   parseOne "lli2eee" ~?= (BenList [BenList [BenInt 2]]),
                   parseOne "lli2eeei2e5:disca" ~?= (BenList [BenList [BenInt 2]]),
                   -- parse dict
                   parseOne "de" ~?= (BenDict []),
                   parseOne "d1:a1:x1:b1:ye" ~?= (BenDict [(BenString "a", BenString "x"), 
                                                           (BenString "b", BenString  "y")])]





main :: IO ()
main = do
       args <- getArgs
       B.readFile (args !! 0) >>= print . parseAll
       