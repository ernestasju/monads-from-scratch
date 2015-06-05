{-# LANGUAGE LambdaCase #-}

module EitherTransExample where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Error
import qualified EitherExample as E

newtype EitherT e m a = EitherT { runEitherT :: m (E.Either e a) }

instance (Error e, Monad m) => Monad (EitherT e m) where
    return = EitherT . return . E.Right
    m >>= k = EitherT (runEitherT m >>= \case
                                          E.Right x -> runEitherT (k x)
                                          E.Left e  -> return (E.Left e))

instance (Error e, Monad m) => MonadPlus (EitherT e m) where
    mzero = EitherT (return (E.Left noMsg))
    m `mplus` n = EitherT $ liftM2 mplus (runEitherT m) (runEitherT n)

mapEitherT :: (m (E.Either e a) -> n (E.Either e b)) -> EitherT e m a -> EitherT e n b
mapEitherT f = EitherT . f . runEitherT

instance (Error e, Functor m, Monad m) => Functor (EitherT e m) where
    fmap = mapEitherT . fmap . fmap

instance (Error e, Functor m, Monad m) => Applicative (EitherT e m) where
    pure = return
    (<*>) = ap

instance (Error e, Functor m, Monad m) => Alternative (EitherT e m) where
    empty = mzero
    (<|>) = mplus

instance Error e => MonadTrans (EitherT e) where
    lift = EitherT . liftM E.Right