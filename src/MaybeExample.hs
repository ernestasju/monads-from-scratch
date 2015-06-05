module MaybeExample where

import Prelude hiding (Maybe(..))
import Control.Monad
import Control.Applicative

data Maybe a = Nothing | Just a deriving (Show)

instance Monad Maybe where
    return = Just
    Just x  >>= k = k x
    Nothing >>= _ = Nothing

instance MonadPlus Maybe where
    mzero = Nothing
    Nothing `mplus` Nothing = Nothing
    Nothing `mplus` Just x  = Just x
    Just x  `mplus` Nothing = Just x
    Just x  `mplus` Just _  = Just x

instance Functor Maybe where
    fmap = liftM

instance Applicative Maybe where
    pure = return
    (<*>) = ap

instance Alternative Maybe where
    (<|>) = mplus
    empty = mzero
