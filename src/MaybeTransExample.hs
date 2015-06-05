{-# LANGUAGE LambdaCase #-}

module MaybeTransExample where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import qualified MaybeExample as M

newtype MaybeT m a = MaybeT { runMaybeT :: m (M.Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . M.Just
    m >>= k = MaybeT (runMaybeT m >>= \case
                                        M.Just x  -> runMaybeT $ k x
                                        M.Nothing -> return M.Nothing)

instance Monad m => MonadPlus (MaybeT m) where
    mzero = MaybeT (return M.Nothing)
    m `mplus` n = MaybeT $ liftM2 mplus (runMaybeT m) (runMaybeT n)

mapMaybeT :: (m (M.Maybe a) -> n (M.Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

instance (Functor m, Monad m) => Functor (MaybeT m) where
    fmap = mapMaybeT . fmap . fmap

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap

instance (Functor m, Monad m) => Alternative (MaybeT m) where
    empty = mzero
    (<|>) = mplus

instance MonadTrans MaybeT where
    lift = MaybeT . liftM M.Just