{-# LANGUAGE DeriveFunctor #-}

module Id where

newtype Id x
  = Id { unId :: x }
  deriving (Functor)
