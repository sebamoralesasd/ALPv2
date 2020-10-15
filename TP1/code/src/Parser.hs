module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "while", "skip"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = chainl1 intexp' (try (do reservedOp lis ","
                                  return ESeq))

intexp' = chainl1 term addopp

term = chainl1 factor multopp

factor = try (parens lis intexp)
         <|> try (do reservedOp lis "-"
                     f <- factor
                     return (UMinus f))
         <|> try (do n <- integer lis
                     return (Const $ fromInteger n))
         <|> try (do str <- identifier lis
                     reservedOp lis "="
                     e <- intexp'
                     return (EAssgn str e))
         <|> try (do str <- identifier lis
                     return (Var str))

addopp = do try (reservedOp lis "+")
            return Plus
         <|> do try (reservedOp lis "-")
                return Minus

multopp = do try (reservedOp lis "*")
             return Times
          <|> do try (reservedOp lis "/")
                 return Div

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolexp' (try (do reservedOp lis "||"
                                    return Or))

boolexp' = chainl1 boolexp'' (try (do reservedOp lis "&&"
                                      return And))

boolexp'' = try (parens lis boolexp)
            <|> try (do reservedOp lis "!"
                        b <- boolexp''
                        return (Not b))
            <|> intComp
            <|> boolValue

intComp = try (do a <- intexp
                  c <- compOp
                  b <- intexp
                  return (c a b))

compOp = try (do reservedOp lis "=="
                 return Eq)
         <|> (do reservedOp lis "!="
                 return NEq)
         <|> (do reservedOp lis "<"
                 return Lt)
         <|> (do reservedOp lis ">"
                 return Gt)

boolValue = try (do reserved lis "true"
                    return BTrue)
            <|> try (do reserved lis "false"
                        return BFalse)
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 comm' (try (do reservedOp lis ";"
                              return Seq))

comm' = try (do reserved lis "skip"
                return Skip)
        <|> try (do reserved lis "if"
                    cond <- boolexp
                    symbol lis "{"
                    case1 <- comm
                    symbol lis "}"
                    reserved lis "else"
                    symbol lis "{"
                    case2 <- comm
                    symbol lis "}"
                    return (IfThenElse cond case1 case2))
        <|> try (do reserved lis "if"
                    cond <- boolexp
                    symbol lis "{"
                    case1 <- comm
                    symbol lis "}"
                    return (IfThen cond case1))
        <|> try (do reserved lis "while"
                    cond <- boolexp
                    symbol lis "{"
                    c <- comm
                    symbol lis "}"
                    return (While cond c))
        <|> try (do str <- identifier lis
                    reservedOp lis "="
                    e <- intexp
                    return (Let str e))

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
