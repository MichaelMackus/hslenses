module Lib where

import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Monoid hiding (Product)
import Control.Applicative

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

type Getting r s a = (a -> Const r a) -> s -> Const r s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

type GetSet  s t a b = forall x. (a -> Product (Const x) Identity b) -> s -> Product (Const x) Identity t
type Getter  s a = forall r. Getting r s a

-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \f s -> set s <$> f (get s)

-- Make a simple getter lens (can be passed to anything that accepts a Getting * s a)
to :: (s -> a) -> Getter s a
to getter f s = Const . getConst $ f (getter s)

-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 f s = case s of
    Left  s1 -> Left  <$> l1 f s1
    Right s2 -> Right <$> l2 f s2

-- Composition for lenses
(@.) :: Lens' b c -> Lens' a b -> Lens' a c
(@.) = flip (.)

-- Modify the target of a lens and return the result.
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l $ (,) <$> f <*> f

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l $ (,) <$> id <*> f

(-=) :: Setting s t Int Int -> Int -> s -> t
(-=) l i s =
    let f = \a -> a - i
    in  over l f s

-- View contents of a lens.
view :: Getting a s a -> s -> a
view l s = getConst (l Const s)

-- Modify the lens using a pure function.
over :: Setting s t a b -> (a -> b) -> s -> t
over l f s = runIdentity $ l (Identity . f) s

-- Const over
set :: Setting s t a b -> b -> s -> t
set l b s = over l (const b) s

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
united :: Lens' s ()
united f s = const s <$> f ()

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = (`appEndo` []) . getConst . l (\x -> Const (Endo (x:)))

preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst . getConst . l (\x -> Const (First (Just x)))

has :: Getting Any s a -> s -> Bool
has l = getAny . getConst . l (\x -> Const (Any True))
