{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module      : MonadUalet
Description : Mónada con soporte para estado, errores, e IO.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definimos la clase de mónadas 'MonadUalet' que abstrae las mónadas con soporte para estado, errores e IO,
y la mónada 'Ualet' que provee una instancia de esta clase.
-}
module MonadUalet (
   Ualet,
   runUalet,
   -- lookupDecl,
   -- lookupTy,
   printUalet,
   -- setLastFile,
   -- getLastFile,
   -- eraseLastFileDecls,
   -- failPosUalet,
   -- failUalet,
   -- addDecl,
   -- addTy,
   catchErrors,
   MonadUalet,
   module Control.Monad.Except,
   module Control.Monad.State,
)
where

import Common

-- import Lang

import Control.Monad.Except
import Control.Monad.State
import Data.List (deleteFirstsBy)
import Errors (Error (..))
import Global
import System.IO

-- * La clase 'MonadUaletm'

{- | La clase de mónadas 'MonadUalet' clasifica a las mónadas con soporte para operaciones @IO@, estado de tipo 'Global.GlEnv', y errores de tipo 'Errors.Error'.

Las mónadas @m@ de esta clase cuentan con las operaciones:
   - @get :: m GlEnv@
   - @put :: GlEnv -> m ()@
   - @throwError :: Error -> m a@
   - @catchError :: m a -> (Error -> m a) -> m a@
   - @liftIO :: IO a -> m a@

y otras operaciones derivadas de ellas, como por ejemplo
   - @modify :: (GlEnv -> GlEnv) -> m ()
-}
class (MonadIO m, MonadState GlEnv m, MonadError Error m) => MonadUalet m

printUalet :: (MonadUalet m) => String -> m ()
printUalet = liftIO . putStrLn

-- setLastFile :: MonadUalet m => FilePath -> m ()
-- setLastFile filename = modify (\s -> s {lfile = filename})

-- getLastFile :: MonadUalet m => m FilePath
-- getLastFile = gets lfile

-- addDecl :: MonadUalet m => Decl Term -> m ()
-- addDecl d = modify (\s -> s { glb = d : glb s, cantDecl = cantDecl s + 1 })
--
-- addTy :: MonadUalet m => Name -> Ty -> m ()
-- addTy n ty = modify (\s -> s { tyEnv = (n,ty) : tyEnv s })

-- eraseLastFileDecls :: MonadUalet m => m ()
-- eraseLastFileDecls = do
--       s <- get
--       let n = cantDecl s
--           (era,rem) = splitAt n (glb s)
--           tyEnv' = deleteTy (map declName era) (tyEnv s)
--       modify (\s -> s {glb = rem, cantDecl = 0, tyEnv = tyEnv'})
--    where deleteTy xs ps = deleteFirstsBy (\x y -> fst x == fst y) ps (map (flip (,) NatTy) xs)
--
-- hasName :: Name -> Decl a -> Bool
-- hasName nm (Decl { declName = nm' }) = nm == nm'
--
-- lookupDecl :: MonadUalet m => Name -> m (Maybe Term)
-- lookupDecl nm = do
--      s <- get
--      case filter (hasName nm) (glb s) of
--        (Decl { declBody=e }):_ -> return (Just e)
--        [] -> return Nothing
--
-- lookupTy :: MonadUalet m => Name -> m (Maybe Ty)
-- lookupTy nm = do
--       s <- get
--       return $ lookup nm (tyEnv s)
--
-- failPosUalet :: MonadUalet m => Pos -> String -> m a
-- failPosUalet p s = throwError (ErrPos p s)
--
-- failUalet :: MonadUalet m => String -> m a
-- failUalet = failPosUalet NoPos

catchErrors :: (MonadUalet m) => m a -> m (Maybe a)
catchErrors c =
   catchError
      (Just <$> c)
      ( \e ->
         liftIO $
            hPutStrLn stderr (show e)
               >> return Nothing
      )

----
-- Importante, no eta-expandir porque GHC no hace una
-- eta-contracción de sinónimos de tipos
-- y Main no va a compilar al escribir `InputT Ualet()`

{- | El tipo @Ualet@ es un sinónimo de tipo para una mónada construida usando dos transformadores de mónada sobre la mónada @IO@.
El transformador de mónad @ExcepT Error@ agrega a la mónada IO la posibilidad de manejar errores de tipo 'Errors.Error'.
El transformador de mónadas @StateT GlEnv@ agrega la mónada @ExcepT Error IO@ la posibilidad de manejar un estado de tipo 'Global.GlEnv'.
-}
type Ualet = StateT GlEnv (ExceptT Error IO)

-- | Esta es una instancia vacía, ya que 'MonadUalet' no tiene funciones miembro.
instance MonadUalet Ualet

-- 'runUalet\'' corre una computación de la mónad 'Ualet' en el estado inicial 'Global.initialEnv'
runUalet' :: Ualet a -> IO (Either Error (a, GlEnv))
runUalet' c = runExceptT $ runStateT c initialEnv

runUalet :: Ualet a -> IO (Either Error a)
runUalet c = fmap fst <$> runUalet' c
