module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let var ie) = do n <- evalExp ie
                           update var n
                           return Skip
stepComm (Seq c1 c2) = do stepComm c1
                          stepComm c2
stepComm (IfThenElse be c1 c2) = do b <- evalExp be
                                    if b then stepComm c1
                                         else stepComm c2
stepComm (While be c) = stepComm (IfThenElse be (Seq c (While be c)) Skip)

-- Dados dos expresiones y un operador, resuelve aplicar el operador a las
-- expresiones y devolver su evaluación.
evalExpOp :: MonadState m => Exp a -> Exp a -> (a -> a -> b) -> m b
evalExpOp e1 e2 operator =  do n1 <- evalExp e1
                               n2 <- evalExp e2
                               return (operator n1 n2)

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const i) = return i
evalExp (Var v) = lookfor v
evalExp (UMinus ie) = do n <- evalExp ie
                         return (-n)
evalExp (Plus ie1 ie2) = evalExpOp ie1 ie2 (+)
evalExp (Minus ie1 ie2) = evalExpOp ie1 ie2 (-)
evalExp (Times ie1 ie2) = evalExpOp ie1 ie2 (*)
evalExp (Div ie1 ie2) = evalExpOp ie1 ie2 div
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt ie1 ie2) = evalExpOp ie1 ie2 (<)
evalExp (Gt ie1 ie2) = evalExpOp ie1 ie2 (>)
evalExp (And be1 be2) = evalExpOp be1 be2 (&&)
evalExp (Or be1 be2) = evalExpOp be1 be2 (||)
evalExp (Not be) = do b <- evalExp be
                      return (not b)
evalExp (Eq ie1 ie2) = evalExpOp ie1 ie2 (==)
evalExp (NEq ie1 ie2) = evalExpOp ie1 ie2 (/=)
evalExp (EAssgn var ie) = do n <- evalExp ie
                             update var n
                             return n
evalExp (ESeq ie1 ie2) = do evalExp ie1
                            evalExp ie2