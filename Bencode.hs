{-# LANGUAGE OverloadedStrings #-}

module Bencode 
    ( MetaInfo
    , BenValue(..)
    , parseOne
    , getFromDict
    , encodeOne
    ) where

import           Control.Applicative              ( (<$>)
                                                  , (<*>)
                                                  , (<|>)
                                                  , (<*)
                                                  , (*>)
                                                  )
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import           Data.ByteString.Char8            (pack)
import           Data.List                        (find)
import           System.Environment               (getArgs)

import qualified Data.Attoparsec.ByteString.Char8 as P
import           Test.HUnit                       ( (~?=)
                                                  , test
                                                  , runTestTT
                                                  )


-- TODO : 
-- put a stricter type on the keys of Dict so that they HAVE to be BenString?
-- Error testing, make sure parsing works on bad data

data BenValue = BenString   String
              | BenInt      Int
              | BenList     [BenValue]
              | BenDict     [(Key, BenValue)]
              deriving (Eq, Show)

type Key = BenValue -- keys for dictionaries must be strings

-- | MetaInfo is a BenDict intended to represent the contents of
-- .torrent files. Note: MetaInfo is not strictly enforced to be a BenDict so
-- you must verify manually that it is a BenDict and also that it does indeed
-- contain all the information from the .torrent file
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
-- kv pairs are not seperated by anything
-- all keys are BenValue Strings
-- values can be any BenValue
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

parseAll :: ByteString -> [BenValue]
parseAll s = extract $ P.parseOnly helper s
  where extract (Right x) = x
        helper = P.manyTill parseExpr P.endOfInput

-- | Parses the ByteString until it has enough to create a BenValue. 
parseOne :: ByteString -> BenValue
parseOne s = extract $ P.parseOnly parseExpr s
  where extract (Right x) = x

-- ................... Accessors .................
-- | Gets the BenValue from the BenDict with the specified key.
-- Produces a Nothing if key is not found
getFromDict :: Key -> BenValue -> Maybe (Key, BenValue)
getFromDict k (BenDict kvs) = getHelper k kvs
  where getHelper :: Key -> [(Key, BenValue)] -> Maybe (Key, BenValue)
        getHelper k kvs = find (\(s,v) -> s == k) kvs
getFromDict _ _             = Nothing

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

-- | Converts a BenValue back to its bencoding as a String
encodeOne :: BenValue -> String
encodeOne b@(BenString _) = encodeString b
encodeOne b@(BenInt _)    = encodeInt b
encodeOne b@(BenList _)   = encodeList b
encodeOne b@(BenDict _)   = encodeDict b

