{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Main
Description : Compilador de Ualet.
Copyright   : (c) Sebastián Morales, 2024.
License     : GPL-3
Maintainer  : scmsasd@gmail.com
Stability   : experimental
-}
module Main where

import System.Console.Haskeline (InputT, defaultSettings, runInputT)

import Control.Exception (IOException, catch)
import Control.Monad.Trans
import Data.Char (isSpace)
import Data.Foldable (forM_)
import System.IO (hPutStrLn, stderr)

import Options.Applicative

import Errors

import Global (GlEnv (..))
import Lang
import Parse (P, parseJournal, runP)

import Eval (evalBalance)
import MonadUalet

prompt :: String
prompt = "Ualet> "

{-
 Tipo para representar las banderas disponibles en línea de comando.
-}
data Mode
  = Interactive
  | Typecheck

-- | Parser de banderas
parseMode :: Parser (Mode, Bool)
parseMode =
  (,)
    <$> ( flag' Typecheck (long "typecheck" <> short 't' <> help "Chequear tipos e imprimir el término")
            <|> flag Interactive Interactive (long "interactive" <> short 'i' <> help "Ejecutar en forma interactiva")
        )
    <*> pure False

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode, Bool, [FilePath])
parseArgs = (\(a, b) c -> (a, b, c)) <$> parseMode <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
 where
  opts =
    info
      (parseArgs <**> helper)
      ( fullDesc
          <> progDesc "Ualet"
          <> header "Aplicación Ualet de la materia ALP."
      )

  go :: (Mode, Bool, [FilePath]) -> IO ()
  go (Interactive, _, files) =
    do
      runUalet (runInputT defaultSettings (repl files))
      return ()

-- go (Typecheck,opt, files) =
--           runOrFail $ mapM_ (typecheckFile opt) files

repl :: (MonadUalet m) => [FilePath] -> InputT m ()
repl args = do
  s <- lift $ catchErrors $ compileFiles args
  Data.Foldable.forM_ s return

compileFiles :: (MonadUalet m) => [FilePath] -> m ()
compileFiles [] = return ()
compileFiles (x : xs) = do
  modify (\s -> s{lfile = x, inter = False})
  _ <- compileFile x
  compileFiles xs

compileFile :: (MonadUalet m) => FilePath -> m ()
compileFile f = do
  printUalet ("Abriendo " ++ f ++ "...")
  let filename = reverse (dropWhile isSpace (reverse f))
  x <-
    liftIO $
      catch
        (readFile filename)
        ( \e -> do
            let err = show (e :: IOException)
            hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
            return ""
        )
  journal <- parseIO filename parseJournal x
  handleJournal journal

parseIO :: (MonadUalet m) => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
  Left e -> throwError (ParseErr e)
  Right r -> return r

handleJournal :: (MonadUalet m) => Journal -> m ()
handleJournal j = do
  printUalet ("Journal parseado: " ++ show j)
  res <- evalBalance j
  printUalet ("Balance: " ++ show res)
  return ()
