{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MonadUalet (
   Ualet,
   runUalet,
   printUalet,
   catchErrors,
   MonadUalet,
   module Control.Monad.Except,
   module Control.Monad.State,
)
where

import Control.Monad.Except
import Control.Monad.State
import Errors (Error (..))
import Global
import System.IO

class (MonadIO m, MonadState GlEnv m, MonadError Error m) => MonadUalet m

printUalet :: (MonadUalet m) => String -> m ()
printUalet = liftIO . putStrLn

catchErrors :: (MonadUalet m) => m a -> m (Maybe a)
catchErrors c =
   catchError
      (Just <$> c)
      ( \e ->
         liftIO $
            hPutStrLn stderr (show e)
               >> return Nothing
      )

type Ualet = StateT GlEnv (ExceptT Error IO)

instance MonadUalet Ualet

-- 'runUalet\'' corre una computación de la mónad 'Ualet' en el estado inicial 'Global.initialEnv'
runUalet' :: Ualet a -> IO (Either Error (a, GlEnv))
runUalet' c = runExceptT $ runStateT c initialEnv

runUalet :: Ualet a -> IO (Either Error a)
runUalet c = fmap fst <$> runUalet' c
