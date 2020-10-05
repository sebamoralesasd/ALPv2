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
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s  = case M.lookup v s of
                    Just x -> x
                    Nothing -> error "Variable no definida dentro del entorno."

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update var i state = M.insert var i state

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Pair Comm State
stepComm (Let var intE) state = let (i :!: newState) = evalExp intE state
                                in (Skip :!: (update var i newState))
stepComm (Seq comm1 comm2) state = case (stepComm comm1 state) of
                                        (Skip :!: newState) -> (comm2 :!: newState)
                                        (c :!: newState) -> ((Seq c comm2) :!: newState)
stepComm (IfThenElse boolE comm1 comm2) state = case (evalExp boolE state) of
                                                     (True :!: newState) -> (comm1 :!: newState)
                                                     (False :!: newState) -> (comm2 :!: newState)
stepComm (While boolE comm) state = case (evalExp boolE state) of
                                         (True :!: newState) -> ((Seq comm (While boolE comm)) :!: newState)
                                         (False :!: newState) -> (Skip :!: newState)

-- Evalua una expresion
evalExp :: Exp a -> State -> Pair a State
evalExp (Const int) state = (int :!: state)
evalExp (Var variable) state = (lookfor variable state :!: state)
evalExp (EAssgn var intE) state = let (i :!: newState) = evalExp intE state
                                  in (i :!: update var i newState)
evalExp (ESeq intE1 intE2) state = let (i1 :!: state1) = evalExp intE1 state
                                       (i2 :!: state2) = evalExp intE2 state1
                                   in (i2 :!: state2)
evalExp (UMinus intE) state = let (i :!: newState) = evalExp intE state
                              in ((-i) :!: newState)
evalExp (Plus intE1 intE2) state = let (i1 :!: state1) = evalExp intE1 state
                                       (i2 :!: state2) = evalExp intE2 state1
                                   in ((i1 + i2) :!: state2)
evalExp (Minus intE1 intE2) state = let (i1 :!: state1) = evalExp intE1 state
                                        (i2 :!: state2) = evalExp intE2 state1
                                    in ((i1 - i2) :!: state2)
evalExp (Times intE1 intE2) state = let (i1 :!: state1) = evalExp intE1 state
                                        (i2 :!: state2) = evalExp intE2 state1
                                    in ((i1 * i2) :!: state2)
evalExp (Div intE1 intE2) state = let (i1 :!: state1) = evalExp intE1 state
                                      (i2 :!: state2) = evalExp intE2 state1
                                  in ((div i1 i2) :!: state2)
evalExp BTrue state = (True :!: state)
evalExp BFalse state = (False :!: state)
evalExp (Lt intE1 intE2) state = let (i1 :!: state1) = evalExp intE1 state
                                     (i2 :!: state2) = evalExp intE2 state1
                                 in ((i1 < i2) :!: state2)
evalExp (Gt intE1 intE2) state = let (i1 :!: state1) = evalExp intE1 state
                                     (i2 :!: state2) = evalExp intE2 state1
                                 in ((i1 > i2) :!: state2)
evalExp (And boolE1 boolE2) state = let (b1 :!: state1) = evalExp boolE1 state
                                        (b2 :!: state2) = evalExp boolE2 state1
                                    in ((b1 && b2) :!: state2)
evalExp (Or boolE1 boolE2) state = let (b1 :!: state1) = evalExp boolE1 state
                                       (b2 :!: state2) = evalExp boolE2 state1
                                   in ((b1 || b2) :!: state2)
evalExp (Not boolE) state = let (b :!: newState) = evalExp boolE state
                            in ((not b) :!: newState)
evalExp (Eq intE1 intE2) state = let (i1 :!: state1) = evalExp intE1 state
                                     (i2 :!: state2) = evalExp intE2 state1
                                 in ((i1 == i2) :!: state2)
evalExp (NEq intE1 intE2) state = let (i1 :!: state1) = evalExp intE1 state
                                      (i2 :!: state2) = evalExp intE2 state1
                                  in ((i1 /= i2) :!: state2)
