module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

main :: IO ()
main = do args <- getArgs
          putStrLn(readExpr(args !! 0))

-- Data type and character definitions
data LispVal = Atom String
          | List [LispVal]
          | DottedList [LispVal] LispVal
          | Number Integer
          | String String
          | Bool Bool
          | Character Char
          
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Main parsing function
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
          Left err -> "No match: " ++ show err
          Right val -> "Found value "

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseBool
          <|> try parseCharacter
          <|> try parseNumber

parseAtom :: Parser LispVal
parseAtom = do 
          first <- letter <|> symbol
          rest <- many (letter <|> digit <|> symbol)
          let atom = [first] ++ rest
          return $ Atom atom

parseString :: Parser LispVal
parseString = do 
          char '"'
          x <- many (noneOf "\"")
          char '"'
          return $ String x

parseBool :: Parser LispVal
parseBool = do 
          x <- try (string "#t" <|> string "#f")
          return $ case x of
                    "#t" -> Bool True
                    "#f" -> Bool False

parseCharacter :: Parser LispVal
parseCharacter = do
          try $ string "#\\"
          -- need special case for space and newline as outlined
          value <- try (string "newline" <|> string "space") 
                    <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
          return $ Character $ case value of
                    "space" -> ' '
                    "newline" -> '\n'
                    otherwise -> (value !! 0)
      
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


