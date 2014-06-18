import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString.Char8 (pack, unpack)
import System.Environment
import Control.Applicative ((<$>), (<*>), (<|>), (<*))
import Control.Monad (liftM)

data BenValue = String String
              | Integer Integer
              | List [BenValue]
              | Dict [(String, BenValue)]
              | Unknown String
              deriving (Eq, Show)

type Metadata = [BenValue]

-- | Parses an int delimited by i and e, i<int>e
-- | "i35e" would be parsed as 35.
-- | Can handle signed ints (leading +/-) so "i-20e" will be parsed as -20
parseInt :: P.Parser BenValue
parseInt = do
          P.char 'i'
          int <- P.signed P.decimal
          P.char 'e'
          return . Integer $ int

-- | Parses a string preceeded by its length and a colon, <#>:<string>
-- | "4:abcd" would be parsed as "abcd" and "4:hello" as "hell"
parseString :: P.Parser BenValue
parseString = do
            n <- P.decimal -- length of string
            P.char ':'
            s <- P.take n 
            return . String . unpack $ s     
       
-- -- does not work
-- parseList :: P.Parser BenValue
-- parseList = P.char 'l' >> pListInternal

-- pListInternal :: P.Parser BenValue
-- pListInternal = do 
--               c <- P.peekChar
--               case c of
--                    Just 'e' -> do
--                                P.char 'e'
--                                return (List [])
--                    _        -> bCons <$> parseExpr <*> parseList 
--               where bCons e (List l) = List (e:l)
          
-- TODO: handle empty case

parseList = do 
          P.char 'l'
          l <- pListInternal
          P.char 'e'
          return l

pListInternal = liftM List $ listWrap <$> parseExpr
           where listWrap x = [x]

parseDict = do
          P.char 'd'
          d <- pDictInternal
          P.char 'e'
          return d

pDictInternal = do
              entries <- P.many' pDictEntry
              return $ Dict entries          

pDictEntry = do
           (String key) <- parseString
           val <- parseExpr
           return $ (key, val)

-- | When no other parser succeeds, the rest of the un-consumed input will
-- | be bundled into an Unknown value as a String
--TODO: should takeWhile not endofinput. right now is just alphanumeric
parseUnknown :: P.Parser BenValue
parseUnknown = do
             s <- P.takeWhile (isAlphaNum)
             return (Unknown $ unpack s)
             where isAlphaNum x = (P.isAlpha_ascii x || P.isDigit x)      

-- | Parses any one of the possible BenValues
parseExpr = parseInt
          <|> parseString
          <|> parseList          
          <|> parseDict
          <|> parseUnknown

-- | Parses an entire string (of metadata) and produces a [BenValue]
parseMeta :: P.Parser Metadata
parseMeta = P.manyTill (parseExpr) (P.endOfInput)

main :: IO ()
main = do
       args <- getArgs
       print $ P.parseOnly parseMeta (pack (args !! 0))