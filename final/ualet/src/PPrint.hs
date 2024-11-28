module PPrint where

import Lang
import MonadUalet (MonadUalet)

import Data.Map
import Data.Text (unpack)
import Data.Time (Year)
import Eval (YearMonth)
import Prettyprinter
import Prettyprinter.Render.Terminal

ingresoColor :: Doc AnsiStyle -> Doc AnsiStyle
ingresoColor = annotate (colorDull Green)
egresoColor :: Doc AnsiStyle -> Doc AnsiStyle
egresoColor = annotate (colorDull Red)
fechaBold :: Doc AnsiStyle -> Doc AnsiStyle
fechaBold = annotate (colorDull White <> bold)
balanceBold :: Doc AnsiStyle -> Doc AnsiStyle
balanceBold = fechaBold
momBold :: Doc AnsiStyle -> Doc AnsiStyle
momBold = fechaBold

entry2doc :: Entry -> Doc AnsiStyle
entry2doc (Entry d des ty m) = sep [fechaBold (pretty (show d)), pretty (show des), entryTipo2doc ty, pretty (show m)]

entryTipo2doc :: Tipo -> Doc AnsiStyle
entryTipo2doc Ingreso = ingresoColor (pretty "ingreso")
entryTipo2doc Egreso = egresoColor (pretty "egreso")

t2doc :: Journal -> Doc AnsiStyle
t2doc (Journal entryList) = vsep $ Prelude.map entry2doc entryList

bal2doc :: Int -> Doc AnsiStyle
bal2doc b = balanceBold (pretty ("Balance: " ++ show b))

ym2doc :: (YearMonth, Int) -> Doc AnsiStyle
ym2doc ((yy, mm), bal) = sep [pretty (show yy ++ "-" ++ show mm ++ ":"), pretty (show bal)]

mom2doc :: Map YearMonth Int -> Doc AnsiStyle
mom2doc mom = vsep $ momBold (pretty "Mes a mes: ") : Prelude.map ym2doc (toList mom)

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

pp :: (MonadUalet m) => Journal -> m String
-- Uncomment to use the Show instance for Journal
{- pp = show -}
pp j = do
  return (render (t2doc j))

ppBalance :: (MonadUalet m) => Int -> m String
ppBalance b = do
  return (render (bal2doc b))

ppMoM :: (MonadUalet m) => Map YearMonth Int -> m String
ppMoM mom = do
  return (render (mom2doc mom))
