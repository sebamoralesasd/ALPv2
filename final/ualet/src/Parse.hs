{- |
Module      : Parse
Description : Define un parser de journal a transacciones.
Copyright   : (c) Sebastián Morales, 2024.
License     : GPL-3
Maintainer  : scmsasd@gmail.com
Stability   : experimental
-}
module Parse (runP, P, parseJournal) where

import Lang
import Prelude hiding (const)

-- import Common
import Text.Parsec hiding (parse, runP)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language -- ( GenLanguageDef(..), emptyDef )

import Data.Time (Day, fromGregorian)

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------

-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer =
        Tok.makeTokenParser $
                emptyDef
                        { commentLine = "#"
                        , reservedNames = ["ingreso", "egreso"]
                        , reservedOpNames = [":", "-"]
                        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parser
-----------------------

parseMonto :: P Int 
parseMonto =  fromInteger <$> natural

parseTipo :: P Tipo
parseTipo =
        (reserved "ingreso" >> return Ingreso)
                <|> (reserved "egreso" >> return Egreso)

parseDescr :: P String
parseDescr = manyTill anyChar (try (reservedOp "-"))

parseDate :: P Day
parseDate = do
        yyyy <- fromInteger <$> natural
        _ <- reservedOp "-"
        mm <- fromInteger <$> natural
        _ <- reservedOp "-"
        dd <- fromInteger <$> natural
        return $ fromGregorian yyyy mm dd

parseEntry :: P Entry
parseEntry = do
        whiteSpace
        fec <- parseDate
        reservedOp "-"
        descr <- parseDescr
        -- reservedOp "-"
        tip <- parseTipo
        reservedOp ":"
        Entry fec descr tip <$> parseMonto

parseJournal :: P Journal
parseJournal = Journal <$> many parseEntry

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s
