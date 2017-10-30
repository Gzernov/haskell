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
      1 -- Tree
      2 -- Const
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

      2 -- Identity
      1, 3, 4 -- Either
  -}

class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m
  fold :: Monoid m => t m -> m
  fold = foldMap id

  {- LAWS
      1. fold      ≡ foldMap id
      2. foldMap f ≡ fold . fmap f

      1 -- default implementation
      2 -- (a, b)
  -}

class (Functor t, Foldable t) => Traversable t where
  sequenceA :: Applicative f => t (f a) -> f (t a)

  {- LAWS
    1. t . sequenceA             ≡ sequenceA . fmap t
    2. sequenceA . fmap Identity ≡ Identity
    3. sequenceA . fmap Compose  ≡ Compose . fmap sequenceA . sequenceA
  -}

class Semigroup a where
  (<>) :: a -> a -> a

newtype Identity a = Identity { runIdentity :: a }

data  Either a b  =  Left a | Right b

data Tree a = Leaf | Node a (Tree a) (Tree a)

newtype Const a b = Const { getConst :: a }

-- instance Monad     m => MonadFish m  where
--   returnFish = return --1
--   (>=>) f g a = f a >>= g --2

  -- LAW : f >=> returnFish ≡ f
  --       f >=> returnFish a ≡ f a >>=  returnFish --2
  --                          ≡ f a >>= return --1
  --                          ≡ f a --LAW 1
  --       f >=> returnFish   ≡ f

-- instance Monad     m => MonadJoin m where
--   returnJoin = return --1
--   join monad = monad >>= id --2

  -- LAW : join . returnJoin      ≡ id
  --       join . returnJoin      ≡ join . return --1
  --                              ≡ return >>= id --2
  --                              ≡ id  --LAW 2

-- instance MonadFish m => Monad     m  where
--   return = returnFish    --1
--   m >>=f = (>=>) id f m  -- 2

  -- LAW : m >>= return     ≡ m
  --       m >>= return     ≡ m >>= returnFish --1
  --                        ≡ (>=>) id returnFish m --2
  --                        ≡ id m --LAW 1
  --                        ≡ m   -- id def


instance MonadFish m => MonadJoin m  where
  returnJoin = returnFish --1
  join = (>=>) id id --2

  -- LAW : join . returnJoin      ≡ id
  --      join . returnJoin       ≡ id (>=>) returnFish --2
  --                              ≡ id       -- LAW 1

instance (Functor m, MonadJoin m) => Monad m where
  return  = returnJoin --1
  m >>= f = join $ fmap f m --2

  -- LAW: m >>= return    ≡ m
  --      m >>= return    ≡ join $ fmap return m --2
  --                      ≡ join $ fmap returnJoin m --1
  --      >>= return      ≡ join . fmap returnJoin --def $
  --                      ≡ id --LAW 2
  --      m >>= return    ≡ m

instance (Functor m, MonadJoin m) => MonadFish m where
  returnFish = returnJoin -- 1
  f >=> g = join . fmap g . f -- 2

  -- LAW : f >=> returnFish     ≡ f
  --                            ≡ f >=> returnJoin  -- 1
  --                            ≡ (join . fmap returnJoin) . f -- 2
  --                            ≡ id . f --2
  --                            ≡ f

instance Functor Identity  where
  fmap f = Identity . f . runIdentity

instance Functor (Either b)  where
  fmap f (Right x) = Right $ f x --1
  fmap _ (Left x)  = Left x

instance Functor Tree  where
  fmap _ Leaf                = Leaf --1
  fmap f (Node v left right) = Node (f v) (fmap f left) (fmap f right) --2

 -- LAW : 1. fmap id         ≡ id
 --          --Base
 --          fmap id Leaf    ≡ Leaf ≡ id  --1
 --          --Inductive step. Proved for l and r => will be proved for (Node v l r)
 --          fmap id Node    ≡ Node (f v) (fmap id left) (fmap id right) --2
 --                          ≡ Node v left right ≡ id

instance Functor (Const a)  where
  fmap _ (Const a) = Const a --1

  -- LAW : 2. fmap f . fmap g ≡ fmap (f . g)
  --          fmap f . fmap g ≡ (\_ -> Const a) . (\_ -> Const b) --1
  --                          ≡ Const a --def .
  --          fmap (f . g)    ≡ fmap h -- h == f . g
  --                          ≡ Const a -- 1

instance Functor ((,) a)  where
  fmap f (a, b) = (a, f b)

instance Applicative Identity  where
  pure = Identity --1
  Identity f <*> val = fmap f val --2

  -- LAW 2. pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
  --        pure (.) <*> u <*> v <*> w ≡ Identity . <*> u <*> v <*> w --1
  --                                   ≡ fmap . <*> u <*> v <*> w, u == Indentity ui --2
  --                                   ≡ Identity $ . $ runIdentity (u <*> v <*> w) --fmap
  --                                   ≡ Identity $ ui $ runIdentity (v <*> w) --fmap
  --
  --                   u <*> (v <*> w) ≡ fmap ui (v <*> w), u == Indentity ui --2
  --                                   ≡ Identity $ ui $ runIdentity (v <*> w) --fmap

instance Applicative (Either a)  where
  pure = Right --1
  Right f <*> val = fmap f val --2
  Left x <*> _ = Left x --3

  -- LAW 1. pure id <*> v ≡ v
  --        pure id <*> v ≡ Right id <*> v --1
  --                      ≡ fmap id v --2
  --                      ≡ v
  -- LAW 3. pure f <*> pure x ≡ pure (f x)
  --        pure f <*> pure x ≡ Right f <*> Right x --1
  --                          ≡ fmap f (Right x) --2
  --                          ≡ Right $ f x -- fmap 1
  --        pure (f x)        ≡ Right (f x)  --1
  -- LAW 4. u <*> pure y      ≡ pure ($ y) <*> u
  --        u <*> pure y      ≡ u <*> Right y --1
  --                          ≡ Left, u == Left --3
  --
  --                          ≡ fmap v (Right y), u == Right v  --2
  --                          ≡ Right $ v y --fmap 2
  --
  --        pure ($ y) <*> u  ≡ Right ($ y) <*> u --1
  --                          ≡ fmap ($ y) u --2
  --                          ≡ Left, u == Left --fmap 2
  --
  --                          ≡ fmap ($ y) (Right v), u == Right v --case Right
  --                          ≡ Right $ ($ y) v --fmap 1
  --                          ≡ Right $ v y --def $


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
  foldMap _ (Left _)  = mempty
  foldMap f (Right x) = f x

instance Foldable Tree where
  foldMap _ Leaf         = mempty
  foldMap f (Node v l r) = f v `mappend` foldMap f l `mappend` foldMap f r

instance Foldable (Const a) where
  foldMap _ _ = mempty

instance Foldable ((,) a) where
  foldMap f (_, v) = f v --1

  -- LAW 2. foldMap f ≡ fold . fmap f
  -- LAW  foldMap f x ≡ fold (fmap f x)
  --  fold (fmap f x) ≡ foldMap id (fmap f x) --fold implementation
  --                  ≡ foldMap id (a, f b) --x == (a, b); fmap
  --                  ≡ id $ f b -- 1
  --                  ≡ f b
  --
  --      foldMap f x ≡ foldMap f (a, b)
  --                  ≡ f b        -- 1

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
