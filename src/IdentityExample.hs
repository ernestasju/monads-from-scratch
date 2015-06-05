module IdentityExample where

import Control.Monad
import Control.Applicative

data Identity a = Identity { runIdentity :: a }

instance Monad Identity where
    return = Identity
    m >>= g = g (runIdentity m)

instance Functor Identity where
    fmap = liftM

instance Applicative Identity where
    pure = return
    (<*>) = ap
