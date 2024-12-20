module Common where

--------------------------
-- Posición en un archivo
--------------------------
type Line = Int
type Column = Int

data Pos
  = -- | No hay info de posición
    NoPos
  | -- | Posición en un archivo.
    Pos !Line !Column

instance Semigroup Pos where
  i <> NoPos = i
  _ <> i = i

instance Monoid Pos where
  mempty = NoPos

instance Show Pos where
  show (Pos line column) = "(" ++ show line ++ "," ++ show column ++ ")"
  show NoPos = ""

---------------------
-- Utility functions
--------------------
abort :: String -> a
abort s = error ("INTERNAL ERROR: " ++ s)

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x
