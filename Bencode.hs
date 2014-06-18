import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import System.Environment
import Control.Applicative ((<$>), (<*>), (<|>), (<*))
import Control.Monad (liftM)

data BenValue = ByteString B.ByteString
              | Integer Integer
              | List [BenValue]
              | Dict [(B.ByteString, BenValue)]
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
            return . ByteString $ s               

-- TODO: if failure to match expr inside list or dict, infinite recursion

parseList :: P.Parser BenValue
parseList = do 
          P.char 'l'
          l <- P.many' parseExpr
          P.char 'e'
          return $ List l

-- | Parses a dictionary delimited by d and e, d<key_value pairs>e
-- | kv pairs are not seperated by anything
-- | all keys are BenValue Strings
-- | values can be any BenValue
parseDict :: P.Parser BenValue
parseDict = do
          P.char 'd'
          d <- pDictInternal
          P.char 'e'
          return d

-- | Parses the internals of the dictionary -- everything between d and e
pDictInternal :: P.Parser BenValue
pDictInternal = do
              entries <- P.many' pDictEntry
              return $ Dict entries          

-- | Parses a single dict entry, or key-value pair
pDictEntry :: P.Parser (B.ByteString, BenValue)
pDictEntry = do
           (ByteString key) <- parseString
           val <- parseExpr
           return $ (key, val)
      
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