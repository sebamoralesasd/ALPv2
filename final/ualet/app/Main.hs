{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Console.Haskeline (InputT, defaultSettings, runInputT)

import Control.Exception (IOException, catch)
import Control.Monad.Trans
import Data.Char (isSpace)
import Data.Foldable (forM_)
import System.IO (hPutStrLn, stderr)

import Options.Applicative

import Errors

import Eval
import Global (GlEnv (..))
import Lang
import MonadUalet
import PPrint (pp, ppBalance, ppMoM)
import Parse (P, parseJournal, runP)

prompt :: String
prompt = "Ualet> "

{-
 Tipo para representar las banderas disponibles en línea de comando.
-}
data Mode
  = MonthOnMonth
  | Balance

-- | Parser de banderas
parseMode :: Parser (Mode, Bool)
parseMode =
  (,)
    <$> ( flag' MonthOnMonth (long "monthonmonth" <> short 'm' <> help "Obtener balance mes a mes de la bitácora")
            <|> flag Balance Balance (long "balance" <> short 'b' <> help "Obtener balance de la bitácora")
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
  go (mode, _, files) =
    do
      _ <- runUalet (runInputT defaultSettings (repl mode files))
      return ()

repl :: (MonadUalet m) => Mode -> [FilePath] -> InputT m ()
repl mode args = do
  s <- lift $ catchErrors $ compileFiles mode args
  forM_ s return

compileFiles :: (MonadUalet m) => Mode -> [FilePath] -> m ()
compileFiles _ [] = return ()
compileFiles mode (x : xs) = do
  modify (\s -> s{lfile = x, inter = False})
  _ <- compileFile mode x
  compileFiles mode xs

compileFile :: (MonadUalet m) => Mode -> FilePath -> m ()
compileFile mode f = do
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
  case mode of
    Balance -> balance journal
    MonthOnMonth -> mom journal

parseIO :: (MonadUalet m) => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
  Left e -> throwError (ParseErr e)
  Right r -> return r

balance :: (MonadUalet m) => Journal -> m ()
balance j = do
  journalText <- pp j
  printUalet journalText
  res <- evalBalance j
  balText <- ppBalance res
  printUalet balText
  return ()

mom :: (MonadUalet m) => Journal -> m ()
mom j = do
  journalText <- pp j
  printUalet journalText
  res <- evalMoM j
  momText <- ppMoM res
  printUalet momText
  return ()
