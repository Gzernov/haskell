{-# LANGUAGE TypeOperators #-}
module Block1
  (
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
    -- (.) :: b ~> c -> a ~> b -> a ~> c
    f@(Partial fun) . g@(Defaulted gfun dval) =
      Partial (\x -> fun (applyOrElse gfun x dval))

unwrap :: (a ~> b) -> (a ~> b)
unwrap f@(Partial _)   = f
unwrap (Defaulted f _) = f

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
