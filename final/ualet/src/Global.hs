{-|
Module      : Global
Description : Define el estado global del compilador
Copyright   : (c) Sebastián Morales, 2024.
License     : GPL-3
Maintainer  : scmsasd@gmail.com 
Stability   : experimental

-}
module Global where

-- import Lang

data GlEnv = GlEnv {
  inter :: Bool,        --  ^ True, si estamos en modo interactivo.
  lfile :: String      -- ^ Último archivo cargado.
  -- lfile :: String,      -- ^ Último archivo cargado.
  -- cantDecl :: Int,      -- ^ Cantidad de declaraciones desde la última carga
  -- glb :: [Decl Term],   -- ^ Entorno con declaraciones globales
  -- tyEnv :: [(Name,Ty)]  -- ^ Entorno de tipado de declaraciones globales
}

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv True "" 
