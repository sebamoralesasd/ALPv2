module Eval2
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

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> case runStateError m s of
                                   Left err -> Left err
                                   Right (v :!: s') -> runStateError (f v) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw err = StateError (\s -> Left err)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case M.lookup v s of
                                     Just x -> Right (x :!: s)
                                     Nothing -> Left UndefVar)
                                    -- Esto está bien? No debería usar THROW?

  update v i = StateError (\s -> Right (() :!: update' v i s))
                           where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval c = case runStateError (stepCommStar c) initEnv of
              Left err -> Left err
              Right (v :!: s) -> Right s

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
evalExpOp :: (MonadState m, MonadError m) => Exp a -> Exp a -> (a -> a -> b) -> m b
evalExpOp e1 e2 operator =  do n1 <- evalExp e1
                               n2 <- evalExp e2
                               return (operator n1 n2)

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp (Const i) = return i
evalExp (Var v) = lookfor v
evalExp (UMinus ie) = do n <- evalExp ie
                         return (-n)
evalExp (Plus ie1 ie2) = evalExpOp ie1 ie2 (+)
evalExp (Minus ie1 ie2) = evalExpOp ie1 ie2 (-)
evalExp (Times ie1 ie2) = evalExpOp ie1 ie2 (*)
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
                      return (not b)
evalExp (Eq ie1 ie2) = evalExpOp ie1 ie2 (==)
evalExp (NEq ie1 ie2) = evalExpOp ie1 ie2 (/=)
evalExp (EAssgn var ie) = do n <- evalExp ie
                             update var n
                             return n
evalExp (ESeq ie1 ie2) = do evalExp ie1
                            evalExp ie2
                            
