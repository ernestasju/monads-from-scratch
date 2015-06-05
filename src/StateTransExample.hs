{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module StateTransExample where

import Control.Monad
import Control.Monad.State (MonadState(..))
import Control.Applicative
import Control.Monad.Trans

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Monad (StateT s m) where
    return x = StateT $ \s -> return (x, s)
    m >>= k = StateT $ \s -> do (x, s') <- runStateT m s
                                runStateT (k x) s'

instance (MonadPlus m) => MonadPlus (StateT s m) where
  mzero = StateT $ const mzero
  m `mplus` n = StateT $ \s -> runStateT m s `mplus` runStateT n s

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f (StateT m) = StateT (fmap f m)

instance (Monad m) => Functor (StateT s m) where
    fmap f m = StateT $ \s -> do
        ~(x, s') <- runStateT m s
        return (f x, s')

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (StateT s m) where
    empty = mzero
    (<|>) = mplus

instance MonadTrans (StateT s) where
    lift c = StateT $ \s -> c >>= (\x -> return (x,s))

instance (Monad m) => MonadState s (StateT s m) where
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

get' :: Monad m => StateT s m s
get' = StateT $ \s -> return (s, s)

put' :: Monad m => s -> StateT s m ()
put' s = StateT $ \_ -> return ((), s)

modify :: MonadState s m => (s -> s) -> m ()
modify f = do s <- get
              put $ f s

evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    ~(a, _) <- runStateT m s
    return a

execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    ~(_, s') <- runStateT m s
    return s'