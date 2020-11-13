module PrettyPrinter
  ( printTerm
  ,     -- pretty printer para terminos
    printType     -- pretty printer para tipos
  )
where

import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )
-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i || isLet i || isAs i || isApp i || isNatOp i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c || isLet c || isAs c || isNatOp c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
-- Sección 6
pp ii vs (Let t1 t2) =
  text "let " <>
  text (vs !! ii) <>
  text " = " <>
  parensIf (isLam t1 || isApp t1 || isLet t1 || isAs t1) (pp ii vs t1) <>
  text " in " <>
  parensIf (isLam t2 || isApp t2 || isLet t2 || isAs t2) (pp (ii+1) vs t2)
--Sección 7
pp ii vs (As ter typ) =
  parensIf (isLam ter || isApp ter || isLet ter || isAs ter) (pp ii vs ter) <>
  text " as " <>
  printType typ
-- Sección 8
pp ii vs Unit = text "unit"
-- Sección 9
pp ii vs (Pair t1 t2) = parens ((pp ii vs t1) <>
                                text ", " <>
                                (pp ii vs t2))
pp ii vs (Fst t) =
  text "fst " <>
  parensIf (isApp t || isLam t || isLet t || isAs t || isPairOp t || isNatOp t) (pp ii vs t)
pp ii vs (Snd t) =
  text "snd " <>
  parensIf (isApp t || isLam t || isLet t || isAs t || isPairOp t || isNatOp t) (pp ii vs t)
-- Sección 10
pp ii vs Zero = text "0"
pp ii vs (Suc t) =
  text "succ " <>
  parensIf (isApp t || isLam t || isLet t || isAs t || isPairOp t || isNatOp t)
   (pp ii vs t)
pp ii vs (Rec t1 t2 t3) =
  text "R " <>
  sep [parensIf (isLam t1 || isApp t1 || isLet t1 || isAs t1) (pp ii vs t1),
   nest 1 (parensIf (isLam t2 || isApp t2 || isLet t2 || isAs t2) (pp ii vs t2)),
   nest 1 (parensIf (isLam t3 || isApp t3 || isLet t3 || isAs t3) (pp ii vs t3))]

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _         = False

isAs :: Term -> Bool
isAs (As _ _) = True
isAs _        = False

isPairOp :: Term -> Bool
isPairOp (Fst _) = True
isPairOp (Snd _) = True
isPairOp _       = False

isNatOp :: Term -> Bool
isNatOp (Suc _)     = True
isNatOp (Rec _ _ _) = True
isNatOp _           = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
-- Sección 8
printType UnitT = text "Unit"
-- Sección 9
printType (PairT t1 t2) = text "(" <>
                          printType t1 <>
                          text ", " <>
                          printType t2 <>
                          text ")"
-- Sección 10
printType NatT = text "Nat"

isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
-- Sección 6
fv (Let t1 t2       ) = fv t1 ++ fv t2
-- Sección 7
fv (As ter typ      ) = fv ter
-- Sección 8
fv Unit               = []
-- Sección 9
fv (Fst t           ) = fv t
fv (Snd t           ) = fv t
fv (Pair t1 t2      ) = fv t1 ++ fv t2
-- Sección 10
fv Zero               = []
fv (Suc t           ) = fv t
fv (Rec t1 t2 t3    ) = fv t1 ++ fv t2 ++ fv t3

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t
