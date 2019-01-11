{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Utils
  ( type (~>)
  , type (~~>)
  ) where

infixr 0 ~>
type f ~> g = forall x. f x -> g x

infixr 0 ~~>
type f ~~> g = forall x y. f x y -> g x y
