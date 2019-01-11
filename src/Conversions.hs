{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conversions where

import Utils
import Id

import Free
import FreeAp
import FreeAr

-- Applicative <-> Arrow

-- Functor `Down`, from Pro to End_Compose

newtype Down f x = Down { unDown :: f () x }

instance Profunctor f => Functor (Down f) where
   fmap f (Down v) = Down (dimap id f v)

instance (Arrow f) => Applicative (Down f) where
  pure x = Down (arr $ const x)
  Down f <*> Down g = Down (g >>> force f)
    where
      force :: (Arrow f) => f () (a -> b) -> f a b
      force f = dimap ((),) apply (first f)
      apply :: (a -> b, a) -> b
      apply (f, a) = f a

phi0Down :: Id ~> Down (->)
phi0Down (Id x) = Down (\_ -> x)

phiDown :: (Arrow f, Arrow g) => Day (Down f) (Down g) ~> Down (PCom f g)
phiDown (Day (Down f) (Down g)) = Down (PCom f (dimap (\f -> ((), f)) (\(z, f) -> f z) (first g)))

mapDown :: (f ~~> g) -> (Down f ~> Down g)
mapDown f (Down x) = Down (f x)

foldApWithAr :: forall f' f x. (Arrow f, Arrow x) =>
  (f' ~> Down f) ->
  ((->) ~~> x) ->
  (PCom f x ~~> x) ->
  (FreeAp f' ~> Down x)
foldApWithAr g i a = foldAp i' (a' . bimapDay g id)
  where
    i' :: Id ~> Down x
    i' = mapDown i . phi0Down
    a' :: Day (Down f) (Down x) ~> Down x
    a' = mapDown a . phiDown

-- Arrow <-> Monad

-- Functor `Kleisli`, from End_Compose -> SPro

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance (Functor f) => Profunctor (Kleisli f) where
  dimap f g (Kleisli v) = Kleisli (fmap g . v . f)

instance (Monad m) => Arrow (Kleisli m) where
  arr xy = Kleisli (return . xy)
  (Kleisli mxy) >>> (Kleisli myz) = Kleisli (\x -> mxy x >>= \y -> myz y)
  first (Kleisli f) = Kleisli (\(x,z) -> (,z) <$> f x)

phi0Kleisli :: (->) ~~> Kleisli Id
phi0Kleisli f = Kleisli (Id . f)

phiKleisli :: (Functor x) => PCom (Kleisli x) (Kleisli y) ~~> Kleisli (Compose x y)
phiKleisli (PCom (Kleisli f) (Kleisli g)) = Kleisli (\x -> Compose (comp (f x) g))
  where comp :: (Functor f) => f x -> (x -> g y) -> f (g y)
        comp fx xgy = fmap (eval . (,xgy)) fx
        eval :: (a, a -> b) -> b
        eval (a, ab) = ab a

mapKleisli :: (f ~> g) -> (Kleisli f ~~> Kleisli g)
mapKleisli h (Kleisli f) = Kleisli (h . f)

foldArWithM :: forall f' f x. (Functor f) =>
  (f' ~~> Kleisli f) ->
  (Id ~> x) ->
  (Compose f x ~> x) ->
  (FreeAr f' ~~> Kleisli x)
foldArWithM g i a = foldAr i' (a'. dimapPCom g id)
  where
    i' :: (->) ~~> Kleisli x
    i' = mapKleisli i . phi0Kleisli
    a' :: PCom (Kleisli f) (Kleisli x) ~~> Kleisli x
    a' = mapKleisli a . phiKleisli

-- Applicative <-> Monad

-- Functor `Id1`, from End_Day -> End_Compose

newtype Id1 f x
  = Id1 { unId1 :: f x }
  deriving (Functor)

instance Applicative m => Applicative (Id1 m) where
  pure = Id1 . pure
  Id1 f <*> Id1 a = Id1 (f <*> a)

phi0Id1 :: Id ~> Id1 Id
phi0Id1 = Id1

phiId1 :: (Functor f, Functor g) => Day (Id1 f) (Id1 g) ~> Id1 (Compose f g)
phiId1 (Day (Id1 fza) (Id1 gz)) = Id1 (Compose (h fza gz))
  where
    h :: (Functor f, Functor g) => f (z -> a) -> g z -> f (g a)
    h fza gz = fmap (\za -> fmap za gz) fza

mapId1 :: (f ~> g) -> (Id1 f ~> Id1 g)
mapId1 f (Id1 x) = Id1 (f x)

foldApWithM :: forall f' f x. (Functor f, Functor x) =>
  (f' ~> Id1 f) ->
  (Id ~> x) ->
  (Compose f x ~> x) ->
  (FreeAp f' ~> Id1 x)
foldApWithM g i a = foldAp i' (a' . bimapDay g id)
  where
    i' :: Id ~> Id1 x
    i' = mapId1 i . phi0Id1
    a' :: Day (Id1 f) (Id1 x) ~> Id1 x
    a' = mapId1 a . phiId1
