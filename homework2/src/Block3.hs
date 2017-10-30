{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}


module Block3
  (
  ) where

import           Prelude (Monoid (mappend, mempty), id, ($), (.))

class Functor f where
  fmap :: (a -> b) -> f a -> f b

  {- LAWS
      1. fmap id         ≡ id
      2. fmap f . fmap g ≡ fmap (f . g)
  -}

class Monad m where
  return     :: a -> m a
  (>>=)      :: m a -> (a -> m b) -> m b

  {- LAWS
      1. m >>= return    ≡ m
      2. return a >>= f  ≡ f a
      3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
  -}

class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

  {- LAWS
      1. f >=> returnFish ≡ f
      2. returnFish >=> f ≡ f
      3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
  -}

class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a

  {- LAWS
      1. join . returnJoin      ≡ id
      2. join . fmap returnJoin ≡ id
      3. join . fmap join       ≡ join . join
      4* join . fmap (fmap f)   ≡ fmap f . join
  -}

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  {- LAWS
      1. pure id <*> v              ≡ v
      2. pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
      3. pure f <*> pure x          ≡ pure (f x)
      4. u <*> pure y               ≡ pure ($ y) <*> u
  -}
class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m

  {- LAWS
      1. fold      ≡ foldMap id
      2. foldMap f ≡ fold . fmap f
  -}

class (Functor t, Foldable t) => Traversable t where
  sequenceA :: Applicative f => t (f a) -> f (t a)

class Semigroup a where
  (<>) :: a -> a -> a

newtype Identity a = Identity { runIdentity :: a }

data  Either a b  =  Left a | Right b

data Tree a = Leaf | Node a (Tree a) (Tree a)

newtype Const a b = Const { getConst :: a }

instance Monad     m => MonadFish m  where
  returnFish = return
  (>=>) f g a = f a >>= g

-- instance Monad     m => MonadJoin m where
--   returnJoin = return
--   join monad = monad >>= id

instance MonadFish m => Monad     m  where
  return = returnFish
  m >>=f = (>=>) id f m

instance MonadFish m => MonadJoin m  where
  returnJoin = returnFish
  join = (>=>) id id

instance Functor Identity  where
  fmap f = Identity . f . runIdentity

instance Functor (Either b)  where
  fmap f (Right x) = Right $ f x
  fmap _ (Left x)  = Left x

instance Functor Tree  where
  fmap f Leaf                = Leaf
  fmap f (Node v left right) = Node (f v) (fmap f left) (fmap f right)

instance Functor (Const a)  where
  fmap f (Const a) = Const a

instance Functor ((,) a)  where
  fmap f (a, b) = (a, f b)

instance Applicative Identity  where
  pure = Identity
  Identity f <*> val = fmap f val

instance Applicative (Either a)  where
  pure = Right
  Right f <*> val = fmap f val
  Left x <*> _ = Left x

instance Semigroup (Tree a) where
  (<>) Leaf t                = t
  (<>) (Node v Leaf right) t = Node v t right
  (<>) (Node v left Leaf) t  = Node v left t
  (<>) (Node v left right) t = Node v (left <> t) right

instance Applicative Tree where
  pure x = Node x Leaf Leaf
  Leaf <*> _ = Leaf
  Node f r l <*> val = fmap f val <> (r <*> val) <> (l <*> val)

instance Monoid m => Applicative (Const m) where
  pure _ = Const mempty
  (Const f) <*> (Const v) = Const (f `mappend` v)

instance Monoid m => Applicative ((,) m) where
  pure x = (mempty, x)
  (x, f) <*> (y, v) = (x `mappend` y, f v)

instance Foldable Identity where
  foldMap f (Identity v) = f v

instance Foldable (Either a) where
  foldMap f (Left x)  = mempty
  foldMap f (Right x) = f x

instance Foldable Tree where
  foldMap f Leaf         = mempty
  foldMap f (Node v l r) = f v `mappend` foldMap f l `mappend` foldMap f r

instance Foldable (Const a) where
  foldMap f _ = mempty

instance Foldable ((,) a) where
  foldMap f (_, v) = f v

instance Traversable Identity where
  sequenceA (Identity f) = pure Identity <*> f

instance Traversable (Either a) where
  sequenceA (Right f) = pure Right <*> f
  sequenceA (Left x)  = pure (Left x)

instance Traversable Tree where
  sequenceA Leaf = pure Leaf
  sequenceA (Node v l r) = -- Tree (f a)
    fmap Node v   -- (a -> Tree a -> Tree a -> Tree a) -> f a -> f (Tree a -> Tree a -> Tree a)
    <*> sequenceA l --f (Tree a -> Tree a -> Tree a) -> f (Tree a) -> f (Tree a -> Tree a)
    <*> sequenceA r --f (Tree a -> Tree a) -> f (Tree a) -> f (Tree a)

instance Traversable (Const a) where
  sequenceA (Const f) = pure (Const f)

instance Traversable ((,) a) where
  sequenceA (x, f) = pure (\y -> (x, y)) <*> f
