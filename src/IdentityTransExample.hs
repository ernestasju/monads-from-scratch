module IdentityTransExample where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import IdentityExample as I

newtype IdentityT m a = IdentityT { runIdentityT :: m (I.Identity a) }

instance Monad m => Monad (IdentityT m) where
    return = IdentityT . return . I.Identity
    m >>= k = IdentityT (runIdentityT m >>= runIdentityT . k . I.runIdentity)

instance Monad m => Functor (IdentityT m) where
    fmap = liftM

instance Monad m => Applicative (IdentityT m) where
    pure = return
    (<*>) = ap

instance MonadTrans IdentityT where
    lift = IdentityT . liftM I.Identity