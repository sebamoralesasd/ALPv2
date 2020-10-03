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
intexp = chainl1 term addopp

term = chainl1 factor multopp

factor = try (parens lis intexp)
         <|> try (do reservedOp lis "-"
                     f <- factor
                     return (UMinus f))
         <|> (do n <- integer lis
                 retrun (Const n)
              <|> do str <- identifier lis
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
                  retrun (c a b))
                  
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
                    -- ~ parse {
                    case1 <- comm
                    -- ~ parse }
                    reserved lis "else"
                    -- ~ parse {
                    case2 <- comm
                    -- ~ parse }
                    return (IfThenElse cond case1 case2))
        <|> try (do reserved lis "if"
                    cond <- boolexp
                    -- ~ parse {
                    case1 <- comm
                    -- ~ parse }
                    return (IfThenElse cond case1 Skip))
        <|> try (do reserved lis "while"
                    cond <- boolexp
                    -- ~ parse {
                    c <- comm
                    -- ~ parse }
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
