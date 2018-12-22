{-# LANGUAGE OverloadedStrings #-} 

module Bibmin.Parse where

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Bibmin.Bibtex (Bibtex(Bibtex)) -- use constructor only
import Data.Char

type Parser = Parsec Void Text

bibtexParser :: Parser [Bibtex]
bibtexParser = between sc eof $ many bibtex

bibtex :: Parser Bibtex
bibtex = Bibtex <$ atmark 
  <*> entry <* lbrace
  <*> key <* comma
  <*> tags <* rbrace
  <?> "bibtex"

entry :: Parser Text
entry = text entryString <?> "entry"
  where
    entryString = some C.letterChar

key :: Parser Text
key = text keyString <?> "key"
  where
    keyString = (:) 
      <$> C.alphaNumChar 
      <*> many (C.satisfy $ (isAlphaNum ||| isPunctuation) &&& (/= ','))

tags :: Parser [(Text, Text)]
tags = sepBy1 tag comma <?> "tags"

tag :: Parser (Text, Text)
tag = (,) <$> text labelString <* equal <*> value <?> "tag"
  where
    labelString = some C.letterChar

value :: Parser Text
value = lexeme $ between dquote dquote valueString
  where
    valueString = T.pack <$> some (C.satisfy $ isPrint &&& (/= '\"'))

-- Combinators

sc :: Parser ()
sc = C.space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

text :: Parser String -> Parser Text
text p = T.pack <$> lexeme p

-- symbols

atmark :: Parser Text
atmark = symbol "@"

comma :: Parser Text
comma = symbol ","

equal :: Parser Text
equal = symbol "="

lbrace :: Parser Text
lbrace = symbol "{"

rbrace :: Parser Text
rbrace = symbol "}"

dquote :: Parser Text
dquote = symbol "\""

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool 
(&&&) p q x = p x && q x
infixr 3 &&&

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p q x = p x || q x 
infixr 2 |||