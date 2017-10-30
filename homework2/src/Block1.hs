{-# LANGUAGE TypeOperators #-}
module Block1
  (
    Expr ( .. ),
    ArithmeticError ( .. ),
    eval
  ) where

import           Control.Category (Category, id, (.))
import           Control.Monad    (liftM2)
import           Data.Maybe       (fromMaybe)

data Expr =
  Const Int
  | Sum Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show, Read)

newtype ArithmeticError = ArithmeticError {msg :: String}
  deriving (Eq, Show, Read)

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Sum a b) = liftM2 (+) (eval a) (eval b)
eval (Sub a b) = liftM2 (-) (eval a) (eval b)
eval (Mul a b) = liftM2 (*) (eval a) (eval b)
eval (Div a b) =
  if eval b == Right 0
  then
    Left $ ArithmeticError "Zero division"
  else
    liftM2 div (eval a) (eval b)
eval (Pow a b) = case eval b of
    Right x ->
      if x < 0
      then
        Left $ ArithmeticError "Negative pow"
      else liftM2 (^) (eval a) (Right x)
    Left y -> Left y


data a ~> b
  = Partial   (a -> Maybe b) -- a partial function
  | Defaulted (a ~> b) b     -- a partial function with a default value
instance Category (~>)
  where
    id = Partial Just
    -- (.) :: (b ~> c) -> (a ~> b) -> (a ~> c)
    f@ (Defaulted func v) . g = Defaulted (func Control.Category.. g) v
    f . g =
      Partial (\x -> sub (apply g x) f )

  -- LAW : id . f = f . id = f
  -- id . g = Partial (\x -> sub (apply g x) id )
  --        = Partial (\x -> sub Nothing id) | apply g x == Nothing
  --        = Partial (Nothing) == result g x
  --
  --        = Partial (\x -> sub Just a id) | apply g x = Just a
  --        = Partial Just a == result g x
  --
  --  f . id = Partial (\x -> sub (apply id x) f)
  --         = Partial (\x -> sub Just x f)
  --         = Partial (\x -> f x)
  --         = Partial f
  --
  -- f@ (Defaulted func v) . id  = Defaulted (func . id) v
  --                            = ...
  --                            = Defaulted (Partial func . id)
  --                            = f

  -- LAW : (f . g) . h = f . (g . h)
  --
  -- (f . g) . h = Partial (\x -> sub (apply h x) (f . g))
  --             = Partial (\x -> sub (apply h x) Partial (\y -> sub (apply g y) f))
  --             = Partial (\x -> sub (h x) Partial (\y -> sub (g y) f))
  --
  -- f . (g . h) = Partial (\x -> sub (apply (g . h) x) f)
  --             = Partial (\x -> sub (apply (Partial (\y -> sub (apply h y) g)) x) f)
  --             = Partial (\x -> sub (sub (apply h x) g) f)

unwrap :: (a ~> b) -> (a -> Maybe b)
unwrap (Partial f)   = f
unwrap (Defaulted f _) = unwrap f

sub :: Maybe a -> (a ~> b) -> Maybe b
sub Nothing (Defaulted f fv) = Just fv
sub Nothing _ = Nothing
sub (Just a) f = unwrap f a

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total f = Partial $ Just Prelude.. f

apply :: (a ~> b) -> a -> Maybe b
apply (Partial f) x     = f x
apply (Defaulted f _) x = apply f x

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f val def = fromMaybe def (apply f val)

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault f@(Partial _)   = Defaulted f
withDefault (Defaulted f _) = withDefault f

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f v = fromMaybe False (apply f v >>= \x -> Just True )

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f@(Defaulted _ _ ) _          = f
orElse (Partial _) g@(Defaulted _ _) = g
orElse f@(Partial _) (Partial _)     = f
