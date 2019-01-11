{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module FreeAp where

import Utils
import Id

-- Day Convolution

data Day (f :: * -> *) (g :: * -> *) (a :: *) where
  Day :: f (z -> a) -> g z -> (f `Day` g) a

instance (Functor f) => Functor (f `Day` g) where
  fmap :: (a -> b) -> (f `Day` g) a -> (f `Day` g) b
  fmap ab (Day fza gz) = Day (fmap (\za z -> ab (za z)) fza) gz

bimapDay :: (f ~> h) -> (g ~> i) -> (Day f g ~> Day h i)
bimapDay m1 m2 (Day fza gz) = Day (m1 fza) (m2 gz)

-- Free Applicative

data FreeAp f x = Pure x | Rec ((f `Day` FreeAp f) x)
  deriving (Functor)

instance (Functor f) => Applicative (FreeAp f) where
  pure :: x -> FreeAp f x
  pure = Pure
  (<*>) :: FreeAp f (a -> b) -> FreeAp f a -> FreeAp f b
  (Pure g) <*> x = fmap g x
  (Rec (Day fza gz)) <*> x = Rec (Day (fmap (\f (z, a) -> f z a) fza) ((,) <$> gz <*> x))

foldAp :: (Id ~> x) -> (Day a x ~> x) -> (FreeAp a ~> x)
foldAp gen _ (Pure x) = gen (Id x)
foldAp gen alg (Rec day) = alg (bimapDay id (foldAp gen alg) day)
