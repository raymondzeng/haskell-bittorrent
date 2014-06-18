import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString.Char8 (pack, unpack)
import System.Environment
import Control.Applicative ((<$>), (<*>), (<|>), (<*))

data BenValue = String String
              | Error String
              | Integer Integer
              | List [BenValue]
              | Dict [(String, BenValue)]
              deriving (Eq, Show)

type Metadata = [BenValue]

parseInt :: P.Parser BenValue
parseInt = do
          P.char 'i'
          int <- P.signed P.decimal
          P.char 'e'
          return $ Integer int

parseString :: P.Parser BenValue
parseString = do
            n <- P.decimal -- length of string
            P.char ':'
            s <- P.take n 
            return $ (String . unpack) s     
       
-- does not work
parseList :: P.Parser BenValue
parseList = do
          P.char 'l'
          pListInternal

pListInternal :: P.Parser BenValue
pListInternal = do 
              c <- P.peekChar
              case c of
                   Just 'e' -> return (List [])
                   _        -> bCons <$> parseExpr <*> parseList 
              where bCons x (List l) = List (x:l)
          
parseExpr = parseInt
          <|> parseString
          <|> parseList           
          <|> parseUnknown

parseUnknown :: P.Parser BenValue
parseUnknown = do
             s <- P.takeWhile (isAlphaNum)
             return (Error $ unpack s)
             where isAlphaNum x = (P.isAlpha_ascii x || P.isDigit x)                   
main :: IO ()
main = do
       args <- getArgs
       print $ P.parseOnly parseMeta (pack (args !! 0))

parseMeta :: P.Parser Metadata
parseMeta = P.manyTill (parseExpr) (P.endOfInput)