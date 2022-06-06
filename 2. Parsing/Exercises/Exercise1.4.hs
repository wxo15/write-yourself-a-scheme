module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

main :: IO ()
main = do args <- getArgs
          putStrLn(readExpr(args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
          Left err -> "No match: " ++ show err
          Right val -> "Found value "
          
spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
          | List [LispVal]
          | DottedList [LispVal] LispVal
          | Number Integer
          | String String
          | Bool Bool

parseString :: Parser LispVal
parseString = do 
          char '"'
          x <- many (noneOf "\"")
          char '"'
          return $ String x
          
parseAtom :: Parser LispVal
parseAtom = do 
          first <- letter <|> symbol
          rest <- many (letter <|> digit <|> symbol)
          let atom = [first] ++ rest
          return $ Atom atom

parseBool :: Parser LispVal
parseBool = do 
          char '#'
          x <- oneOf ("tf")
          return $ case x of
                    't' -> Bool True
                    'f' -> Bool False
           
parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseOthers

-- case when no radix is given. Assume decimal, same as before
parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

-- other cases, when radix is given
parseOthers :: Parser LispVal
parseOthers = do
          char '#'
          base <- oneOf("dbox")
          case base of
                    'd' -> do
                              x <- many1 digit
                              (return . (Number . read)) x
                    'b' -> do
                              x <- many1 (oneOf "01")
                              (return . (Number . bin2dec)) x
                    'o' -> do
                              x <- many1 (oneOf "01234567")
                              (return . (Number . oct2dec)) x
                    'x' -> do
                              x <- many1 (oneOf "0123456789abcdef")
                              (return . (Number . hex2dec)) x

bin2dec = bin2d 0
bin2d dec "" = dec
bin2d dec (x:xs) = bin2d (2 * dec + read([x])) xs
oct2dec :: String -> Integer
oct2dec x = fst $ readOct x !! 0
hex2dec :: String -> Integer
hex2dec x = fst $ readHex x !! 0

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseBool

