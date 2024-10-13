module Eval where

import Control.Monad (foldM)
import Lang
import MonadUalet (MonadUalet)

evalBalance :: (MonadUalet m) => Journal -> m Int
evalBalance (Journal entrylist) = foldM evalSumEntry 0 entrylist

evalSumEntry :: (MonadUalet m) => Int -> Entry -> m Int
evalSumEntry acc (Entry _ _ Ingreso m) = return (acc + m)
evalSumEntry acc (Entry _ _ Egreso m) = return (acc - m)
