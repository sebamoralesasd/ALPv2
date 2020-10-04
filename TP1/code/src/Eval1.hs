module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty
-- ~ fromlist []

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s  = case M.lookup v s of
                    Just x -> x
                    Nothing -> error "variable no definida"

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update var i state = (M.update (\x -> Just x) var state) i

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
-- ~ (:!:) :: a -> b -> Pair a b
-- ~ (a:!:b) = (a,b)
stepComm (Let var intE) state = 
stepComm (Seq comm1 comm2) state = 
stepComm (IfThenElse boolE comm1 comm2) state = 
stepComm (While boolE comm) state = 

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const int) state =
evalExp (Var variable) state =
evalExp (EAssgn var intE) state =
evalExp (ESeq intE1 intE2) state =
evalExp (UMinus intE) state =
evalExp (Plus intE1 intE2) state =
evalExp (Minus intE1 intE2) state =
evalExp (Times intE1 intE2) state =
evalExp (Div intE1 intE2) state =
evalExp BTrue state =
evalExp BFalse state =
evalExp (Lt intE1 intE2) state =
evalExp (Gt intE1 intE2) state =
evalExp (And boolE1 boolE2) state =
evalExp (Or boolE1 boolE2) state =
evalExp (Not boolE) state =
evalExp (Eq intE1 intE2) state =
evalExp (NEq intE1 intE2) state =
