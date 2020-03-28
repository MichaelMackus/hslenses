module Lenses where

import Control.Applicative
import Data.Bifunctor

import Lib

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f a = (, snd a) <$> f (fst a)

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f b = (fst b,) <$> f (snd b)

_abs :: Real a => Lens' a a
_abs f n = update <$> f (abs n)
  where
    update x
      | x < 0     = error "_abs: absolute value can't be negative"
      | otherwise = signum n * x
