{-# LANGUAGE GADTs #-}

module Parse where

import Core

import Data.Functor
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Partial = NoOp | Op (Prop -> Prop -> Prop) Prop

spaceConsumer =
  L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

-- spaceWithoutNewline :: Parser ()
-- spaceWithoutNewline = eof <|> c ' ' <|> c '\t'
--   where c x = char x >> pure ()

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

parseProgramText :: Parser [Text]
parseProgramText = do
  spaceConsumer
  some parseGoalText

parseGoalText :: Parser Text
parseGoalText = do
  text <- some $ satisfy (\x -> x /= ';')
  lexeme $ string ";"
  pure $ pack text

parseProgram = do
  spaceConsumer
  some parseGoal

parseGoal = do
  env <- parseProp `sepBy1` lexeme (string ",")
  o "|-" <|> o "⊢" <|> try (o "proves") <|> try (o "leads to")
  concl <- parseProp
  lexeme $ char ';'
  pure $ Goal env concl

-- operator precedence levels
-- ¬    3
-- ∧    2
-- ∨    1
-- ⇒ ⇔  0

parsePartial :: Parser Text -> (Prop -> Prop -> Prop) -> Parser Prop -> Parser Partial
parsePartial opparse opconstr parserLevel = do
  op <- opparse
  rest <- parserLevel
  pure $ Op opconstr rest

parseProp :: Parser Prop
parseProp = lexeme $
  try parseTrue   <|>
  try parseFalse  <|>
  parseConditional

assembleOp :: Prop -> Partial -> Prop
assembleOp p NoOp = p
assembleOp p (Op opconstr q) = opconstr p q

o x = lexeme $ string x

parseConditional = assembleOp <$> parseOr <*> parsePartialConditional
parsePartialConditional =
  parsePartial (o "->" <|> o "→" <|> o "⇒" <|> try (o "implies")) Implies parseConditional <|>
  parsePartial (o "<->" <|> o "↔" <|> o "⇔" <|> try (o "iff")) Iff parseConditional <|>
  parseEmpty

parseOr = assembleOp <$> parseAnd <*> parsePartialOr
parsePartialOr =
  parsePartial (o "|" <|> o "∨" <|> try (o "or")) Or parseOr <|>
  parseEmpty

parseAnd = assembleOp <$> parseAtomic <*> parsePartialAnd
parsePartialAnd =
  parsePartial (o "&" <|> try (o "and") <|> o "∧") And parseAnd <|>
  parseEmpty

parseEmpty = string "" >> pure NoOp

parseAtomic =
  parseNot <|>
  parseVar <|>
  parseParenthesized

parseParenthesized = do
  lexeme $ char '('
  body <- parseProp
  lexeme $ char ')'
  pure body

parseTrue =  lexeme (string "T" <|> string "True"  <|> string "true")  $> PropTrue
parseFalse = lexeme (string "F" <|> string "False" <|> string "false") $> PropFalse

parseNot = do
  o "~" <|> o "¬" <|> try (o "not")
  p <- parseProp
  pure $ Not p

parseVar = Var <$> parseName

parseName = lexeme $ try $ do
  first <- letterChar
  rest <- many parseNameChar
  pure . pack $ first : rest

parseNameChar =
  letterChar <|>
  digitChar  <|>
  char '_'   <|>
  char '\''
