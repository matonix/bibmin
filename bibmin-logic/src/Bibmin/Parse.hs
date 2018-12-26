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
    keyString = (:) <$> C.alphaNumChar <*> many keyChar
    keyChar = satisfy $ (isAlphaNum ||| isPunctuation) &&& isNot ','

tags :: Parser [(Text, Text)]
tags = sepBy1 tag comma <?> "tags"

tag :: Parser (Text, Text)
tag = (,) <$> text labelString <* equal <*> lexeme value <?> "tag"
  where
    labelString = some C.letterChar

value :: Parser Text
value = numberValue
  <|> letterValue
  <|> dquoteValue
  <|> braceValue
  <?> "value"
  where
    numberValue = text $ some C.numberChar
    letterValue = text $ some C.letterChar
    dquoteValue = between dquote dquote latexString
    braceValue = between lbrace rbrace latexString

latexString :: Parser Text
latexString = T.concat <$> many (brace <|> bare)
  where
    brace = cat <$> single '{' <*> many latexSequence <*> single '}'
      where
        cat x y z =  x `T.cons` T.concat y `T.snoc` z
    bare = T.concat <$> some latexSequence

latexSequence :: Parser Text
latexSequence = escapeSequence <|> latexChar
  where
    latexChar :: Parser Text
    latexChar = T.singleton <$> satisfy (isPrint &&& isNoneOf "{}\"")
    escapeSequence :: Parser Text
    escapeSequence = cat <$> single '\\' <*> C.printChar
      where
        cat x y = T.pack [x, y]

-- Combinators

sc :: Parser ()
sc = C.space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

text :: Parser String -> Parser Text
text p = T.pack <$> lexeme p

-- Symbols

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

-- Char region

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p q x = p x && q x
infixr 3 &&&

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p q x = p x || q x
infixr 2 |||

isNot :: Char -> Char -> Bool
isNot = (/=)

isNoneOf :: String -> Char -> Bool
isNoneOf = flip notElem