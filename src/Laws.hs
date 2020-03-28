module Laws where

import Lib
import Traversals
import Data.Functor.Compose

-- testTraversal :: Applicative f => ((a -> f b) -> s -> f t) -> (forall f'. Applicative f' => a -> f' b) -> (forall f'. Applicative f' => a -> f' b) -> s -> Bool
-- testTraversal t f g s = traversal1 t f g s == traversal2 t f g s

-- fmap (t f) . t g = getCompose . t (Compose . fmap f . g)
traversal1 t f g = fmap (t f) . t g
traversal2 t f g = getCompose . t (Compose . fmap f . g)
