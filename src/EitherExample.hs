-- {-# LANGUAGE InstanceSigs #-}
module EitherExample where

import Prelude hiding (Either(..))
import Control.Monad
import Control.Applicative
import Control.Monad.Error

data Either e a = Left e | Right a deriving (Show)

instance Error e => Monad (Either e) where
    return = Right
    Right x >>= k = k x
    Left e >>= _ = Left e

    fail = Left . strMsg

instance Error e => MonadPlus (Either e) where
    mzero = Left noMsg
    Left _  `mplus` n = n
    Right x `mplus` _ = Right x

instance Error e => Functor (Either e) where
    fmap = liftM

instance Error e => Applicative (Either e) where
    pure = return
    (<*>) = ap

instance (Error e) => Alternative (Either e) where
    (<|>) = mplus
    empty = mzero