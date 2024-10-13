{-# LANGUAGE DeriveFunctor #-}

module Lang where

import Data.Time (Day)

newtype Journal = Journal [Entry] deriving (Show)

data Entry = Entry
  { fecha :: Day
  , descripcion :: String
  , tipo :: Tipo
  , monto :: Int 
  }
  deriving (Show)

data Tipo = Ingreso | Egreso deriving (Show)
