module Main where

import Control.Applicative ( Alternative (..) )
import Control.Monad       ( ap )
import Data.Char           ( isDigit, isLower, isUpper, isSpace )

newtype Parser a = Parser { parse :: String -> Maybe(a, String) }

item :: Parser Char
item = Parser $ \input -> case input of
                            ""     -> Nothing
                            (x:xs) -> Just (x,xs)

instance Monad Parser where
    return :: a -> Parser a
    return = pure 

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f  = Parser $ \input -> do
                            (x,output) <- parse p input
                            parse (f x) output

instance Applicative Parser where 
    pure :: a -> Parser a
    pure x = Parser $ \input -> Just (x,input)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) = ap

instance Functor Parser where 
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = pure f <*> p

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing 

    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \input -> do parse p1 input <|> parse p2 input

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do { x <- item; if p x then return x else empty }

munch :: (Char -> Bool) -> Parser String
munch = many . satisfy

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = traverse char

stringLit :: Parser String
stringLit = char '"' *> munch (/= '"') <* char '"'

stringEsc :: Parser String
stringEsc = undefined

upper :: Parser Char
upper = satisfy isUpper

lower :: Parser Char
lower = satisfy isLower

digit :: Parser Char
digit = satisfy isDigit

ws :: Parser Char
ws = satisfy isSpace
