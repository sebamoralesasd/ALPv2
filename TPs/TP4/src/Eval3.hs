module Eval3
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva monada que lleve el costo de las 
-- operaciones efectuadas en la computacion, ademas de manejar errores y 
-- estado, y de su instancia de mónada. Llamela StateErrorCost.
newtype StateErrorCost a =
  StateErrorCost { 
    runStateErrorCost :: Env -> Either Error ( Pair (a, Cost) Env)
    }

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorCost where
  fmap = liftM

instance Applicative StateErrorCost where
  pure  = return
  (<*>) = ap

instance Monad StateErrorCost where
  return x = StateErrorCost (\s -> Right ((x, 0) :!: s))
  m >>= f = StateErrorCost (\s -> 
    case runStateErrorCost m s of
      Left err -> Left err
      Right ((v, c) :!: s') -> 
        case runStateErrorCost (f v) s' of
          Left err' -> Left err'
          Right ((v', c') :!: s2) -> Right ((v', c+c') :!: s2) )

-- Ejercicio 3.c: Dar una instancia de MonadCost para StateErrorCost.
instance MonadCost StateErrorCost where
  tick = StateErrorCost (\s -> Right (((), 1) :!: s) )

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorCost.
instance MonadError StateErrorCost where
  throw error = StateErrorCost (\_ -> Left error)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorCost.
instance MonadState StateErrorCost where
  lookfor v = StateErrorCost (\s -> case M.lookup v s of 
    Just x -> Right ((x, 0) :!: s)
    Nothing -> Left UndefVar)
  
  update v i = StateErrorCost (\s -> Right (((), 0) :!: update' v i s))
    where update' = M.insert

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorCost.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error (Env, Cost)
eval c = 
  case runStateErrorCost (stepCommStar c) initEnv of
    Left error -> Left error
    Right ((_, c) :!: s) -> Right (s, c)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar :: (MonadState m, MonadError m, MonadCost m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
-- stepComm :: [dar el tipo segun corresponda]
stepComm :: (MonadState m, MonadError m, MonadCost m) => Comm -> m Comm
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

-- Evalua una expresion 
evalExpOp :: (MonadState m, MonadError m, MonadCost m) => Exp a -> Exp a -> (a -> a -> b) -> m b
evalExpOp e1 e2 operator =  do n1 <- evalExp e1
                               n2 <- evalExp e2
                               tick
                               return (operator n1 n2)

evalExp :: (MonadState m, MonadError m, MonadCost m) => Exp a -> m a
evalExp (Const i) = return i
evalExp (Var v) = lookfor v
evalExp (UMinus ie) = do n <- evalExp ie
                         tick
                         return (-n)
evalExp (Plus ie1 ie2) = evalExpOp ie1 ie2 (+)
evalExp (Minus ie1 ie2) = evalExpOp ie1 ie2 (-)
evalExp (Times ie1 ie2) = do tick
                             evalExpOp ie1 ie2 (*)                
evalExp (Div ie1 ie2) = do n1 <- evalExp ie1
                           n2 <- evalExp ie2
                           if n2 == 0 then throw DivByZero
                                      else return (div n1 n2)
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt ie1 ie2) = evalExpOp ie1 ie2 (<)
evalExp (Gt ie1 ie2) = evalExpOp ie1 ie2 (>)
evalExp (And be1 be2) = evalExpOp be1 be2 (&&)
evalExp (Or be1 be2) = evalExpOp be1 be2 (||)
evalExp (Not be) = do b <- evalExp be
                      tick
                      return (not b)
evalExp (Eq ie1 ie2) = evalExpOp ie1 ie2 (==)
evalExp (NEq ie1 ie2) = evalExpOp ie1 ie2 (/=)
evalExp (EAssgn var ie) = do n <- evalExp ie
                             update var n
                             return n
evalExp (ESeq ie1 ie2) = do evalExp ie1
                            evalExp ie2
