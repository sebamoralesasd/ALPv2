module Eval where

import Control.Monad (foldM)
import Data.Map (Map, fromListWith)
import qualified Data.Map as Map
import Data.Time (Day, toGregorian)
import Lang (Entry (Entry), Journal (..), Tipo (Egreso, Ingreso))
import MonadUalet (MonadUalet)

evalBalance :: (MonadUalet m) => Journal -> m Int
evalBalance (Journal entrylist) = foldM evalSumEntry 0 entrylist

evalMoM :: (MonadUalet m) => Journal -> m (Map YearMonth Int)
evalMoM j = return $ monthOnMonth j

evalSumEntry :: (MonadUalet m) => Int -> Entry -> m Int
evalSumEntry acc e = return (sumEntry acc e)

sumEntry :: Int -> Entry -> Int
sumEntry acc (Entry _ _ Ingreso m) = acc + m
sumEntry acc (Entry _ _ Egreso m) = acc - m

monthOnMonth :: Journal -> Map YearMonth Int
monthOnMonth = calcMoM . groupByMonth

type YearMonth = (Integer, Int) -- (año, mes)

entryDay :: Entry -> Day
entryDay (Entry f _ _ _) = f

calcMoM :: Map YearMonth [Entry] -> Map YearMonth Int
calcMoM = Map.map calc
 where
  calc = Prelude.foldl sumEntry 0

groupByMonth :: Journal -> Map YearMonth [Entry]
groupByMonth (Journal entrylist) = fromListWith (++) entries
 where
  entries = [(getYearMonth (entryDay entry), [entry]) | entry <- entrylist]
  getYearMonth :: Day -> YearMonth
  getYearMonth day = let (yy, mm, _) = toGregorian day in (yy, mm)
