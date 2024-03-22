module Combinators where

import Control.Applicative ( Alternative (..) )
import Control.Monad       ( ap )
import Data.Char           ( isAlpha, isDigit, isLower, isUpper, isSpace )

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
satisfy p = do 
    x <- item
    if p x then return x else empty

munch :: (Char -> Bool) -> Parser String
munch = many . satisfy

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep >> p) <|> return []

endBy :: Parser a -> Parser b -> Parser [a]
endBy p end = many $ do
    xs <- p
    end
    return xs

noneOf :: String -> Parser Char
noneOf cs = satisfy (`notElem` cs)

between :: Parser open -> Parser close -> Parser a -> Parser a
between = undefined

letter :: Parser Char
letter = satisfy isAlpha

word :: Parser String
word = some letter

char :: Char -> Parser Char
char = satisfy . (==)

string :: Parser String
string = undefined

string' :: String -> Parser String
string' = traverse char

stringLit :: Parser String
stringLit = do
    char '"' 
    xs <- munch (/= '"') 
    char '"'
    return xs

stringEsc :: Parser String
stringEsc = undefined

upper :: Parser Char
upper = satisfy isUpper

lower :: Parser Char
lower = satisfy isLower

digit :: Parser Char
digit = satisfy isDigit

nat :: Parser Int
nat = do 
    ds <- some digit
    return (read ds)

fl :: Parser Float
fl = undefined

int :: Parser Int
int = do 
    char '-'
    n <- nat
    return (-n)
    <|> nat

ws :: Parser Char
ws = satisfy isSpace

space :: Parser ()
space = do
    many ws
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

-- NEEDED: floats, strings with white-spaces, escaped strings, UTF-8 support
