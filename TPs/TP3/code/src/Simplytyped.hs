module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n    )    = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u  )    = conversion' b t :@: conversion' b u
conversion' b (LAbs n t u)    = Lam t (conversion' (n : b) u)
-- Sección 6
conversion' b (LLet n t1 t2)  = Let (conversion' b t1) (conversion' (n:b) t2)
-- Sección 7
conversion' b (LAs t typ)     = As (conversion' b t) typ
-- Sección 8
conversion' b LUnit           = Unit
-- Sección 9
conversion' b (LPair t1 t2)   = Pair (conversion' b t1) (conversion' b t2)
conversion' b (LFst t)        = Fst (conversion' b t)
conversion' b (LSnd t)        = Snd (conversion' b t)
-- Sección 10
conversion' b LZero           = Zero
conversion' b (LSuc t)        = Suc (conversion' b t)
conversion' b (LRec t1 t2 t3) = Rec (conversion' b t1) (conversion' b t2) (conversion' b t3)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
-- Sección 6
sub i t (Let t1 t2)           = Let (sub i t t1) (sub (i+1) t t2)
-- Sección 7
sub i t (As t' typ)           = As (sub i t t') typ
-- Sección 8
sub i t Unit                  = Unit
-- Sección 9
sub i t (Pair t1 t2)          = Pair (sub i t t1) (sub i t t2)
sub i t (Fst t')              = Fst (sub i t t')
sub i t (Snd t')              = Snd (sub i t t')
-- Sección 10
sub i t Zero                  = Zero
sub i t (Suc t')              = Suc (sub i t t')
sub i t (Rec t1 t2 t3)        = Rec (sub i t t1) (sub i t t2) (sub i t t3)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval _ (Lam      t   u      ) = VLam t u
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2) = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1)
eval e (u        :@: v      ) = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Error de tipo en run-time, verificar type checker"
-- Sección 6
eval e (Let t1 t2           ) = let value = eval e t1
                                 in eval e (sub 0 (quote value) t2)
-- Sección 7
eval e (As t typ            ) = eval e t
-- Sección 8
eval e Unit                   = VUnit
-- Sección 9
eval e (Pair t1 t2          ) = VPair (eval e t1) (eval e t2)
eval e (Fst t1              ) = case eval e t1 of
                                (VPair v1 v2) -> v1
                                _ -> error "Error de tipo en run-time, verificar type checker"
eval e (Snd t1              ) = case eval e t1 of
                                (VPair v1 v2) -> v2
                                _ -> error "Error de tipo en run-time, verificar type checker"
-- Sección 10
eval e Zero                   = VNum NZero
eval e (Suc t)                = case eval e t of
                                VNum n -> VNum (NSuc n)
                                _      -> error "Error de tipo en run-time, verificar type checker"
eval e (Rec t1 t2 t3)         = case eval e t3 of
                                VNum NZero    -> eval e t1
                                VNum (NSuc n) -> eval e ((t2 :@: (quote (eval e (Rec t1 t2 (quote (VNum n)))))) :@: (quote (VNum n)))

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f)      = Lam t f
--Sección 8
quote VUnit           = Unit
-- Sección 9
quote (VPair v1 v2)   = Pair (quote v1) (quote v2)
-- Sección 10
quote (VNum NZero)    = Zero
quote (VNum (NSuc n)) = Suc (quote (VNum n))

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- Sección 9
tupleError :: Type -> Either String Type
tupleError t = err $ "Función de tupla aplicado a un tipo "
                     ++ render (printType t) ++ "."

succError :: Type -> Either String Type
succError t = err $ "Sucesor aplicado a un tipo" ++ render (printType t) ++ "."

-- Sección 10
rError1 :: Type -> Type -> Either String Type
rError1 t1 t2 = err $ "Error de tipado en operacion R. Tipo del primer argumento: "
                    ++ render (printType t1)
                    ++ ". Tipo del segundo argumento: "
                    ++ render (printType t1) ++ "."

rError2 :: Type -> Either String Type
rError2 t = err $ "Operador R con tercer argumento de tipo "
                ++ render (printType t)
                ++ " cuando se espera un tipo Nat."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
-- Sección 6
infer' c e (Let t1 t2) = infer' c e t1 >>= (\tt1 -> infer' (tt1:c) e t2)
-- Sección 7
infer' c e (As t typ) = infer' c e t >>=
                        (\tt -> if tt == typ then ret typ else matchError typ tt)
-- Sección 8
infer' c e Unit = ret UnitT
-- Sección 9
infer' c e (Pair t1 t2) = infer' c e t1 >>=
                          (\tt1 -> infer' c e t2 >>=
                          (\tt2 -> ret (PairT tt1 tt2)))
infer' c e (Fst t) = infer' c e t >>=
                     (\tt -> case tt of
                             PairT t1 t2 -> ret t1
                             x -> tupleError x)
infer' c e (Snd t) = infer' c e t >>=
                     (\tt -> case tt of
                             PairT t1 t2 -> ret t2
                             x -> tupleError x)
-- Sección 10
infer' c e Zero = ret NatT
infer' c e (Suc t) = infer' c e t >>= (\tt -> case tt of
                                              NatT -> ret NatT
                                              x -> succError x)
infer' c e (Rec t1 t2 t3) =
  infer' c e t1 >>=
    (\tt1 -> infer' c e t2 >>=
      (\tt2 -> case tt2 of
               FunT t1A (FunT NatT t1B) -> if t1A == tt1 && t1B == tt1
                                         then infer' c e t3 >>=
                                           (\tt3 -> case tt3 of
                                                    NatT -> ret tt1
                                                    x    -> rError2 x
                                           )
                                         else rError1 tt1 (FunT t1A (FunT NatT t1B))
               t' -> rError1 tt1 t'
      )
    )
----------------------------------
