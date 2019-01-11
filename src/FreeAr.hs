{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module FreeAr where

import Utils

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> (p b c -> p a d)

class Profunctor a => Arrow a where
  arr :: (x -> y) -> a x y
  (>>>) :: a x y -> a y z -> a x z
  first :: a x y -> a (x, z) (y, z)

instance Profunctor (->) where
  dimap a b c = b . c . a

instance Arrow (->) where
  arr = id
  (>>>) = flip (.)
  first f (x, z) = (f x, z)

-- Profunctor Composition

data PCom (p :: * -> * -> *) (q :: * -> * -> *) (x :: *) (y :: *) where
  PCom :: p x z -> q z y -> (p `PCom` q) x y

dimapPCom :: (a ~~> b) -> (c ~~> d) -> (PCom a c ~~> PCom b d)
dimapPCom m1 m2 (PCom a c) = PCom (m1 a) (m2 c)

-- Free Arrow

data FreeAr a x y = Hom (x -> y) | Comp ((a `PCom` FreeAr a) x y)

instance Profunctor a => Profunctor (FreeAr a) where
  dimap f g (Hom h) = Hom (g . h .f)
  dimap f g (Comp (PCom x y)) = Comp (PCom (dimap f id x) (dimap id g y))

instance Arrow a => Arrow (FreeAr a) where
  arr :: (x -> y) -> FreeAr a x y
  arr = Hom
  (>>>) :: FreeAr a x z -> FreeAr a z y -> FreeAr a x y
  (Hom f) >>> c = dimap f id c
  (Comp (PCom x y)) >>> c = Comp (PCom x (y >>> c))
  first :: FreeAr a x y -> FreeAr a (x, z) (y, z)
  first (Hom f) = Hom (\(x, z) -> (f x, z))
  first (Comp (PCom pxz qzy)) = Comp (PCom (first pxz) (first qzy))

foldAr :: ((->) ~~> b) -> (PCom f b ~~> b) -> (FreeAr f ~~> b)
foldAr gen _ (Hom g) = gen g
foldAr gen alg (Comp pcom) = alg (dimapPCom id (foldAr gen alg) pcom)
