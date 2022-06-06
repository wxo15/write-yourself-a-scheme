module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Array

main :: IO ()
main = do args <- getArgs
          putStrLn(readExpr(args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

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
          | Vector (Array Int LispVal)

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
          return $ case atom of
                    "#t" -> Bool True
                    "#f" -> Bool False
                    otherwise -> Atom atom
                  
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
          head <- endBy parseExpr spaces
          tail <- char '.' >> spaces >> parseExpr
          return $ DottedList head tail
          
parseQuoted :: Parser LispVal
parseQuoted = do
          char '\''
          x <- parseExpr
          return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
          char '`'
          x <- parseExpr
          return $ List [Atom "quasiquote", x]

parseUnQuoted :: Parser LispVal
parseUnQuoted = do
          char ','
          x <- parseExpr
          return $ List [Atom "unquote", x]

parseUnQuotedSplicing:: Parser LispVal
parseUnQuotedSplicing = do
          char ','
          char '@'
          x <- parseExpr
          return $ List [Atom "unquote-splicing", x]

parseVector:: Parser LispVal
parseVector = do
          x <- sepBy parseExpr spaces
          return $ Vector (listArray (0,(length x - 1)) x)

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> parseQuasiQuoted
          <|> parseUnQuoted
          <|> parseUnQuotedSplicing
          <|> try (do
                    string "#("
                    x <- parseVector
                    char ')'
                    return x)
          <|> do
                    char '('
                    x <- (try parseList) <|> parseDottedList
                    char ')'
                    return x
