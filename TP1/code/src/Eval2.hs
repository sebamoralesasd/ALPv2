module Eval2
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
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s  = case M.lookup v s of
                    Just x -> Right x
                    Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update var i state = M.insert var i state

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Let var intE) state = case evalExp intE state of
                                     Left error -> Left error
                                     Right (i :!: newState) -> Right (Skip :!: (update var i newState))
stepComm (Seq comm1 comm2) state = case (stepComm comm1 state) of
                                        Left error -> Left error
                                        Right (Skip :!: newState) -> Right (comm2 :!: newState)
                                        Right (c :!: newState) -> Right ((Seq c comm2) :!: newState)
stepComm (IfThenElse boolE comm1 comm2) state = case (evalExp boolE state) of
                                                     Left error -> Left error
                                                     Right (True :!: newState) -> Right (comm1 :!: newState)
                                                     Right (False :!: newState) -> Right (comm2 :!: newState)
stepComm (While boolE comm) state = case (evalExp boolE state) of
                                         Left error -> Left error
                                         Right (True :!: newState) -> Right ((Seq comm (While boolE comm)) :!: newState)
                                         Right (False :!: newState) -> Right (Skip :!: newState)

-- ~ Dado un posible par y una operacion unaria, devuelve un posible par
-- ~ con la operación aplicada y el estado actualizado
handleUnExpr :: Either Error (Pair a State) -> (a -> a) -> Either Error (Pair a State)
handleUnExpr (Left error) _ = Left error
handleUnExpr (Right (val :!: state)) f = Right (f val :!: state)

-- ~ Dadas dos expreciones, el estado original y una operacion binaria,
-- ~ devuelve un posible par con la operación aplicada y el estado 
-- ~ actualizado
handleBinExpr :: Exp a -> Exp a -> State -> (a -> a -> b) -> Either Error (Pair b State)
handleBinExpr exp1 exp2 state f = case evalExp exp1 state of
                                  Left error -> Left error
                                  Right (i1 :!: state1) -> case evalExp exp2 state1 of
                                                            Left error -> Left error
                                                            Right (i2 :!: state2) -> Right (f i1 i2 :!: state2)

-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const int) state = Right (int :!: state)
evalExp (Var variable) state = case lookfor variable state of 
                               Left error -> Left error
                               Right i -> Right (i :!: state)
evalExp (EAssgn var intE) state = case evalExp intE state of
                                       Left error -> Left error
                                       Right (i :!: newState) -> Right (i :!: update var i newState)
evalExp (ESeq intE1 intE2) state = handleBinExpr intE1 intE2 state (\a b -> b)
evalExp (UMinus intE) state = handleUnExpr (evalExp intE state) (\x -> (-x))
evalExp (Plus intE1 intE2) state = handleBinExpr intE1 intE2 state (\a b -> a + b)
evalExp (Minus intE1 intE2) state = handleBinExpr intE1 intE2 state (\a b -> a - b)
evalExp (Times intE1 intE2) state = handleBinExpr intE1 intE2 state (\a b -> a * b)
evalExp (Div intE1 intE2) state = case evalExp intE1 state of
                                   Left error -> Left error
                                   Right (i1 :!: state1) -> case evalExp intE2 state1 of
                                                            Left error -> Left error
                                                            Right (0 :!: state2) -> Left DivByZero
                                                            Right (i2 :!: state2) -> Right ((div i1 i2) :!: state2)
evalExp BTrue state = Right (True :!: state)
evalExp BFalse state = Right (False :!: state)
evalExp (Lt intE1 intE2) state =  handleBinExpr intE1 intE2 state (\a b -> a < b)
evalExp (Gt intE1 intE2) state =  handleBinExpr intE1 intE2 state (\a b -> a > b)
evalExp (And boolE1 boolE2) state = handleBinExpr boolE1 boolE2 state (\a b -> a && b)
evalExp (Or boolE1 boolE2) state = handleBinExpr boolE1 boolE2 state (\a b -> a || b)
evalExp (Not boolE) state = handleUnExpr (evalExp boolE state) (\a -> not a)
evalExp (Eq intE1 intE2) state = handleBinExpr intE1 intE2 state (\a b -> a == b)
evalExp (NEq intE1 intE2) state = handleBinExpr intE1 intE2 state (\a b -> a /= b)
