-- | Main entry point to the application.
module Main where

import Control.Applicative
import Control.Monad.Error
--import Control.Monad.State (MonadState(..))
import qualified IdentityExample as I
import qualified IdentityTransExample as It
import qualified MaybeExample as M
import qualified MaybeTransExample as Mt
import qualified EitherExample as E
import qualified EitherTransExample as Et
import qualified StateExample as S
import qualified StateTransExample as St

iTest :: I.Identity String
iTest = (++) <$> pure "Hello, " <*> pure "World!"

mTest :: M.Maybe String
mTest = (++) <$> pure "Hello, " <*> pure "World!"

sTest :: S.State Int String
sTest = do S.modify (+ 1)
           liftM show S.get

data ErrFlag = JustErr | StrErr String deriving (Show)

instance Error ErrFlag where
    noMsg = JustErr
    strMsg = StrErr

eTest, eTest2 :: E.Either ErrFlag String
eTest = (++) <$> pure "Hello, " <*> fail "Error" --pure "World!"
eTest2 = (++) <$> pure "Hello, " <*> pure "World!"

itTest :: It.IdentityT IO String
itTest = (++) <$> pure "Hello, " <*> lift getLine  -- <*> pure "World!"

mtTest :: Mt.MaybeT IO String
mtTest = (++) <$> pure "Hello, " <*> lift getLine  -- <*> pure "World!"

etTest :: Et.EitherT ErrFlag IO String
etTest = (++) <$> pure "Hello, " <*> (lift getLine >>= \xs -> (guard . not . null) xs >> return xs)  -- <*> pure "World!"

stTest :: St.StateT Int IO String
stTest = do k <- lift getLine
            St.modify (+ read k)
            liftM show St.get'

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"

    putStrLn $ "Identity said: " ++ I.runIdentity iTest
    putStrLn $ "Maybe said: " ++ show mTest
    putStrLn $ "Either said: " ++ show eTest
    putStrLn $ "Either said (2): " ++ show eTest2
    putStrLn $ "State said: " ++ show (S.runState sTest 999)

    itTestOutput <- It.runIdentityT itTest
    putStrLn $ "IdentityT said: " ++ I.runIdentity itTestOutput

    mtTestOutput <- Mt.runMaybeT mtTest
    putStrLn $ "MaybeT said: " ++ show mtTestOutput

    etTestOutput <- Et.runEitherT etTest
    putStrLn $ "EitherT said: " ++ show etTestOutput

    stTestOutput <- St.runStateT stTest 9999
    putStrLn $ "StateT said: " ++ show stTestOutput