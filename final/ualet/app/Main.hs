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

import Control.Monad.Catch (MonadMask)
import System.Console.Haskeline (InputT, defaultSettings, runInputT)

-- import Control.Monad

import Control.Exception (IOException, catch)
import Control.Monad.Trans
import Data.Char (isSpace)
import System.IO (hPutStrLn, stderr)

-- import System.Exit

-- import System.Process ( system )
import Options.Applicative

-- import Data.Text.Lazy (unpack)

import Errors
import Global (GlEnv (..))
import Lang
import Parse (P, parseJournal, runP)

-- import Elab ( elab )
-- import Eval ( eval )
-- import PPrint ( pp , ppTy, ppDecl )
import MonadUalet

-- import TypeChecker ( tc, tcDecl )

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

repl :: (MonadUalet m, MonadMask m) => [FilePath] -> InputT m ()
repl args = do
  s <- lift $ catchErrors $ compileFiles args
  case s of
    Nothing -> return ()
    Just x -> return x

compileFiles :: (MonadUalet m) => [FilePath] -> m ()
compileFiles [] = return ()
compileFiles (x : xs) = do
  modify (\s -> s{lfile = x, inter = False})
  compileFile x
  compileFiles xs

compileFile :: (MonadUalet m) => FilePath -> m Lang.Journal
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
  -- decls <- parseIO filename parseJournal x
  parseIO filename parseJournal x

-- mapM_ handleDecl decls

parseIO :: (MonadUalet m) => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
  Left e -> throwError (ParseErr e)
  Right r -> return r

-- typecheckDecl :: MonadUalet m => Decl NTerm -> m (Decl Term)
-- typecheckDecl (Decl p x t) = do
--         let dd = (Decl p x (elab t))
--         tcDecl dd
--         return dd
--
-- handleDecl ::  MonadUalet m => Decl NTerm -> m ()
-- handleDecl d = do
--         (Decl p x tt) <- typecheckDecl d
--         te <- eval tt
--         addDecl (Decl p x te)
