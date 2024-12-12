module Global where

data GlEnv = GlEnv
  { inter :: Bool --  ^ True, si estamos en modo interactivo.
  , lfile :: String
  -- ^ Último archivo cargado.
  }

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv True ""
