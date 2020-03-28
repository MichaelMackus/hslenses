{-# LANGUAGE
  MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, CPP, DefaultSignatures, UndecidableInstances, TypeFamilies #-}

module Traversals where

import Data.Bitraversable
import Data.Tree (Tree(..))
import Data.Sequence (Seq(..))
import qualified Data.Traversable as T

import Lib

-- traversal of nth element
ix :: Int -> Traversal' [a] a
-- ix :: Applicative f => Int
--        -> (a -> f a) -> [a] -> f [a]
ix i f s
    | i < 0     = error "Index must >= 0"
    | null s    = pure s
    | otherwise = if i == 0 then
                    (: drop 1 s) <$> f (head s)
                  else
                    let (h:rest) = s
                    in (h:) <$> ix (i - 1) f (rest)

-- traversal of all a's in list
-- it's not a proper traversal because it violates the composition law
-- 2 all's in a row might not be looking at the same elements
-- (for instance, if you traverse all 0s and change them to 2s, then the next time you'll look at all' 0 you won't see anything)
_all :: Eq a => a -> Traversal' [a] a
_all ref f s = traverse update s
  where update = \old -> if old == ref then f old else pure old

_both :: Bitraversable r => Traversal (r a a) (r b b) a b
_both f s = bitraverse f f s

_head :: Traversal' [a] a
_head f []     = pure []
_head f (x:xs) = (:) <$> f x <*> pure xs

_last :: Traversal' [a] a
_last f [] = pure []
_last f s  = (init s ++) <$> (:[]) <$> f (last s)

_init :: Traversal' [a] [a]
_init f []     = pure []
_init f (x:[]) = pure [x]
_init f s      = (++ [last s]) <$> f (init s)

_tail :: Traversal' [a] [a]
_tail f []     = pure []
_tail f (x:[]) = pure [x]
_tail f (x:xs) = (x:) <$> f xs

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b
#ifndef HLINT
  default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b
  each = T.traverse
  {-# INLINE each #-}
#endif

instance Each [a] [b] a b
-- instance Each (NonEmpty a) (NonEmpty b) a b instance Each (Identity a) (Identity b) a b
instance Each (Maybe a) (Maybe b) a b
instance Each (Seq a) (Seq b) a b
instance Each (Tree a) (Tree b) a b

instance (a~a', b~b') => Each (a,a') (b,b') a b where
  each f ~(a,b) = (,) <$> f a <*> f b
  {-# INLINE each #-}
