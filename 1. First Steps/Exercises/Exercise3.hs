module Main where
import System.Environment

main :: IO ()
main = do putStrLn("What is your name?")
          line <- getLine
          putStrLn("Hello, " ++ line)
