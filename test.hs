import Data.Functor.Identity
import Data.Functor.Product
import Control.Applicative

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

type Getting r s a = (a -> Const r a) -> s -> Const r s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

type GetSet  s t a b = forall x. (a -> Product (Const x) Identity b) -> s -> Product (Const x) Identity t
type Getter  s a = forall r. Getting r s a

-- View contents of a lens.
view :: Getting a s a -> s -> a
view l s = getConst (l Const s)

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

