module StateExample where

import Control.Monad
import Control.Applicative

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    m >>= k = State $ \s -> let (x, s') = runState m s
                            in runState (k x) s'

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure = return
    (<*>) = ap

get :: State s s
get   = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

evalState :: State s a -> s -> a
evalState m s = fst $ runState m s

execState :: State s a -> s -> s
execState m s = snd $ runState m s