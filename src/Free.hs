{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Free where

import Utils
import Id

import Control.Monad

-- Compose: (Endo-)Functor Composition

data Compose (f :: * -> *) (g :: * -> *) (x :: *)
  = Compose { unCompose :: f (g x) }

instance (Functor f, Functor g) => Functor (f `Compose` g) where
  fmap :: (a -> b) -> (f `Compose` g) a -> (f `Compose` g) b
  fmap f (Compose fga) = Compose ((\ga -> f <$> ga) <$> fga)

bimapCompose :: (Functor f) => (f ~> h) -> (g ~> i) -> (Compose f g ~> Compose h i)
bimapCompose m1 m2 = Compose . m1 . fmap m2 . unCompose

-- Free Monad

data Free f x
  = Ret x
  | Con ((f `Compose` Free f) x)
  deriving (Functor)

instance (Functor f) => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance (Functor f) => Monad (Free f) where
  return :: x -> Free f x
  return = Ret
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (Ret x) >>= f = f x
  (Con (Compose m)) >>= f = Con (Compose (fmap (>>= f) m))

foldFree :: (Functor f) => (Id ~> b) -> (Compose f b ~> b) -> (Free f ~> b)
foldFree gen _ (Ret x) = gen (Id x)
foldFree gen alg (Con comp) = alg (bimapCompose id (foldFree gen alg) comp)
