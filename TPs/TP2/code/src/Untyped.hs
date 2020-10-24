module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion  :: LamTerm -> Term
conversion l = convaux [] l

convaux :: [String] -> LamTerm -> Term
convaux xs (LVar s) = case elemIndex s xs of
                      Nothing -> Free (Global s)
                      Just n -> Bound n
convaux xs (App l1 l2) = (convaux xs l1) :@: (convaux xs l2)
convaux xs (Abs s l) = Lam (convaux (s:xs) l)

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) val = f val
vapp (VNeutral neu) val = VNeutral (NApp neu val)

lookfor :: Name -> NameEnv Value -> Value
lookfor name env = case lookup name env of
                     Just value -> value
                     Nothing -> error "Variable no definida dentro del entorno."

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free name) (gEnv, _) = lookfor name gEnv
eval' (t1 :@: t2) env = vapp (eval' t1 env) (eval' t2 env)
eval' (Lam t) (gEnv, lEnv) = VLam (\x-> (eval' t (gEnv, x:lEnv)))

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = quote' 0

quote' :: Int -> Value -> Term
quote' i (VLam f) = let fresh = VNeutral $ NFree (Quote i)
                    in Lam $ quote' (i+1) (f fresh)
quote' i (VNeutral neu) = neuQuote i neu

neuQuote :: Int -> Neutral -> Term
neuQuote i (NFree name) =
  case name of
    (Global str) -> Free $ Global str
    (Quote k) -> Bound (i-k-1)
neuQuote i (NApp neu val) = (quote' i (VNeutral neu)) :@: (quote' i val)
