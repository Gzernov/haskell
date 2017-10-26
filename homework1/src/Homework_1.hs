{-# LANGUAGE ExplicitForAll #-}

module Homework_1
    ( order3
    , highestBit
    , highestBitExtended
    , smartReplicate
    , contains
    -- second block
    , removeAt
    , removeAtExtended
    , collectEvery
    , stringSum
    , mergeSort
    -- third block
    , Day (..)
    , nextDay
    , afterDays
    , isWeekend
    , daysToParty
    , Hp (..)
    , Attack (..)
    , RestHp (..)
    , Monster (..)
    , Knight (..)
    , fight
    , Vector (..)
    , toListFromVector
    , vecLen
    , vecSum
    , vecScalar
    , vecVec
    , vecDist
    , Nat (..)
    , fromIntegerToNat
    , fromNatToInteger
    , Tree (..)
    , insertInTree
    , findInTree
    , sizeOf
    , isEmpty
    , fromList
    -- 4th block
    , toList
    , splitOn
    -- 5th block
    , maybeConcat
    , NonEmpty (..)
    , Identity (..)
    ) where
import           Data.Bits      (countLeadingZeros, finiteBitSize)
import           Data.List      (sort)
import           Data.Semigroup (Semigroup (..))
import           System.Random  (newStdGen, randomRs)
import           TreePrinters

--Block 2 Task 4
randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

-- Block 1
order3 :: (Int, Int, Int) -> (Int, Int, Int)
order3 (x, y, z) = (\[a,b,c] -> (a, b, c)) $ sort [x,y,z]

highestBit :: Int -> Int
highestBit = fst . highestBitExtended

highestBitExtended :: Int -> (Int, Int)
highestBitExtended x = (\z -> (\y -> (2 ^ y, y)) $ max (finiteBitSize z - countLeadingZeros z - 1) 0) $ max x 0

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\y -> replicate y y)

contains :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem

-- Block 2
removeAt :: Int -> [a] -> [a]
removeAt i a = snd (removeAtExtended i a)

removeAtExtended :: Int -> [a] -> (Maybe a, [a])
removeAtExtended index lst =
  let (f, s) = splitAt index lst
  in mergeAndGetHead f s
    where
      mergeAndGetHead :: [a] -> [a] -> (Maybe a, [a])
      mergeAndGetHead f []      = (Nothing, f)
      mergeAndGetHead f (sh:st) = (Just sh, f ++ st)

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery k lst =
  split [] lst []
  where
    split :: [a] -> [a] -> [a] -> ([a], [a])
    split tl [] dropped = (tl, dropped)
    split tl hd dropped =
      let (t, h, d) = moveList (splitAt (k - 1) hd) dropped
      in split (tl ++ t) h d
      where
        moveList :: ([a], [a]) -> [a] -> ([a], [a], [a])
        moveList (_tl, []) _dropped   = (_tl, [], _dropped)
        moveList (_tl, x:xs) _dropped = (_tl, xs, _dropped ++ [x])

--Extended and base
stringSum :: String -> Integer
stringSum x = sum $ map advancedRead $ words x
  where
    advancedRead :: String -> Integer
    advancedRead ('+':xs) = read xs
    advancedRead charr    = read charr

mergeSort :: Ord a => [a] -> [a]
mergeSort []      = []
mergeSort y@(x:_) =
  case y of
    [f, s] -> [min f s, max f s]
    [_] -> [x]
    _ -> let (first, second) = splitList y
      in sortLists (mergeSort first) (mergeSort second) []
      where
        sortLists :: Ord a => [a] -> [a] -> [a] -> [a]
        sortLists a@(ah:atail) b@(bh:bt) res =
          if ah < bh
          then
            sortLists atail b (res ++ [ah])
          else
            sortLists a bt (res ++ [bh])
        sortLists [] b res = res ++ b
        sortLists a [] res = res ++ a

        splitList :: [a] -> ([a], [a])
        splitList _x = splitAt (length _x `div` 2) _x

--Block 3
--first
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Enum, Show, Read)

nextDay :: Day -> Day
nextDay x = afterDays x 1

afterDays :: Day -> Int -> Day
afterDays d x = toEnum $ mod (fromEnum d + x) 7

isWeekend :: Day -> Bool
isWeekend day = fromEnum day > 5

daysToParty :: Day -> Int
daysToParty day = mod (fromEnum Fri - fromEnum day + 7) 7

--second
newtype Hp = Hp {hp :: Int}
  deriving (Show, Eq)
newtype Attack = Attack {at ::Int}
  deriving (Show, Eq)
newtype RestHp = RestHp Hp
  deriving (Show, Eq)
data Monster = Monster Hp Attack
  deriving (Show, Eq)
data Knight = Knight Hp Attack
  deriving (Show, Eq)

fight :: Monster -> Knight -> (Either Monster Knight, RestHp)
fight (Monster mhp mat) (Knight khp kat) =
  let
    mAttacks = hp khp `div` at mat + 1
    kAttacks = (hp mhp + 1) `div` at kat
    knightRest = Hp (hp khp `mod` (mAttacks * at mat))
    monsterRest = Hp (hp mhp `mod` (kAttacks * (at kat - 1)))
  in
  if kAttacks < mAttacks
  then
    (Right (Knight knightRest kat), RestHp knightRest)
  else
    (Left (Monster monsterRest mat), RestHp monsterRest)

--third
data Vector a = Vector2D a a | Vector3D a a a
  deriving (Show)

toListFromVector :: Num a => Vector a -> [a]
toListFromVector (Vector2D x y)   = [x, y, 0]
toListFromVector (Vector3D x y z) = [x, y, z]

fromListToVector :: Num a => Eq a => [a] -> Vector a
fromListToVector []        = Vector2D 0 0
fromListToVector [_]       = Vector2D 0 0
fromListToVector [_, _]    = Vector2D 0 0
fromListToVector [x, y, 0] = Vector2D x y
fromListToVector [x, y, z] = Vector3D x y z
fromListToVector (x:y:z:_) = Vector3D x y z

vecLen :: Floating a => Vector a -> a
vecLen v = sqrt $ sum $ map (**2) (toListFromVector v)

vecSum :: Num a => Eq a => Vector a -> Vector a -> Vector a
vecSum v u = fromListToVector $ zipWith (+) (toListFromVector v) (toListFromVector u)

vecScalar :: Num a => Eq a => Vector a -> Vector a -> a
vecScalar v u = sum $ zipWith (*) (toListFromVector v) (toListFromVector u)

vecVec :: Num a => Eq a => Vector a -> Vector a -> Vector a
vecVec v u =
  let
    [vx, vy, vz] = toListFromVector v
    [ux, uy, uz] = toListFromVector u
  in
    fromListToVector [vy * uz - vz * uy, vz * ux - vx * uz, vx * uy - vy * ux]

vecDist ::  Floating a => Eq a => Vector a -> Vector a -> a
vecDist v u = sqrt $ sum $ map (**2) (zipWith (-) (mapDiv2 $ toListFromVector v) (mapDiv2 $ toListFromVector u))
  where
    mapDiv2 :: Floating a => [a] -> [a]
    mapDiv2 = map (/2)

--forth
data Nat = Z | S Nat
  deriving Show
instance Num Nat where
  (+) = ifGOE sumNat (flip sumNat)
  (*) = ifGOE mulNat (flip mulNat)
  (-) = ifGOE subNat (\_ _ -> Z)
  abs = id
  signum _ = S Z
  fromInteger y =
    if signum y == 1
    then
      fromIntegerToNat y Z
    else
      Z
instance Eq Nat where
  x == y = cmpNat x y == EQ
instance Ord Nat where
  compare = cmpNat

ifGOE :: (Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat) -> Nat -> Nat ->  Nat
ifGOE f g x y =
  if x >= y
  then
    f x y
  else
    g x y

cmpNat :: Nat -> Nat -> Ordering
cmpNat Z Z         = EQ
cmpNat Z (S _)     = LT
cmpNat (S _) Z     = GT
cmpNat (S x) (S y) = cmpNat x y

sumNat :: Nat -> Nat -> Nat
sumNat x (S y) = sumNat (S x) y
sumNat x Z     = x

mulNat :: Nat -> Nat -> Nat
mulNat _ Z     = Z
mulNat x (S Z) = x
mulNat x (S y) =  mulNat (sumNat x x) y

subNat :: Nat -> Nat -> Nat
subNat Z (S _)     = Z
subNat x Z         = x
subNat (S x) (S y) =  subNat x y

fromIntegerToNat :: Integer -> Nat -> Nat
fromIntegerToNat 0 n = n
fromIntegerToNat a n = fromIntegerToNat (a - 1) (S n)

fromNatToInteger :: Nat -> Integer
fromNatToInteger Z     = 0
fromNatToInteger (S x) = fromNatToInteger x + 1

--fifth
-- data Tree a = Leaf | Node a (Tree a) (Tree a)
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

sizeOf :: Tree a -> Int
sizeOf (Node _ a b) = sizeOf a + sizeOf b + 1
sizeOf Leaf         = 0

findInTree :: Ord a => Tree a -> a -> Maybe a
findInTree Leaf _ = Nothing
findInTree (Node val left right) x =
  case compare val x of
    EQ -> Just val
    GT -> findInTree left x
    LT -> findInTree right x

insertInTree :: Ord a => Tree a -> a -> Tree a
insertInTree Leaf x = Node x Leaf Leaf
insertInTree t@(Node val left right) x
  | val == x = t
  | val > x =
    case left of
      Leaf -> Node val (Node x Leaf Leaf) right
      _    -> Node val (insertInTree left x) right
  | otherwise =
    case right of
      Leaf -> Node val left (Node x Leaf Leaf)
      _    -> Node val left (insertInTree right x)

fromList :: Ord a => [a] -> Tree a
fromList lst = fromListToTree lst Leaf
  where
    fromListToTree :: Ord a => [a] -> Tree a -> Tree a
    fromListToTree [] x        = x
    fromListToTree (x:xs) tree = fromListToTree xs (insertInTree tree x)


-- 4th block
--first
instance Foldable Tree where
  foldMap = treeFoldMap
  foldr = treeFoldr

treeFoldMap :: Monoid m => (a -> m) -> Tree a -> m
treeFoldMap f tree = foldMap f $ toList tree

treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr func zero tree = foldr func zero (toList tree)

toList :: Tree a -> [a]
toList t = reverse . snd $ getLst (t, [])
  where
    getLst :: (Tree a, [a]) -> (Tree a, [a])
    getLst (Leaf, lst) = (Leaf, lst)
    getLst (tree@(Node v _ _), lst) =
      let
        (least, newTree) = findLeast (v, tree)
        in getLst (newTree, least:lst)
          where
            findLeast :: (a, Tree a) -> (a, Tree a)
            findLeast (_v, Leaf) = (_v, Leaf)
            findLeast (_, Node _val Leaf _right) = (_val, _right)
            findLeast (_v, Node _val _left _right) =
              let
                (resVal, resTree) =  findLeast(_v, _left)
              in (resVal, Node _val resTree _right)

--second
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = foldr (addFunc x) [[]]
  where
    addFunc :: Eq a => a -> a -> [[a]] -> [[a]]
    addFunc y _ [] = [[y]]
    addFunc y cur res@(h:t) =
      if cur == y
        then []:res
        else (cur:h):t

--Fifth
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = concatMap fromMaybe
  where
    fromMaybe :: Maybe [a] -> [a]
    fromMaybe Nothing  = []
    fromMaybe (Just x) = x

data NonEmpty a = a :| [a]
  deriving (Show, Eq)
instance Semigroup (NonEmpty a) where
  (ah :| atail) <> (bh :| btail) = ah :| (atail ++ bh : btail)

newtype Identity a = Identity { runIdentity :: a }
  deriving Show
instance Monoid a => Monoid (Identity a) where
  mempty = mempty
  mappend a b = Identity (mappend (runIdentity a) (runIdentity b))

instance Semigroup (Tree a) where
  (<>) = mappend

instance Monoid (Tree a) where
  mempty = Leaf
  mappend = flip treeMappend

treeMappend :: Tree a -> Tree a -> Tree a
treeMappend Leaf t                = t
treeMappend (Node v Leaf right) t = Node v t right
treeMappend (Node v left Leaf) t  = Node v left t
treeMappend (Node v left right) t = Node v (treeMappend left t) right
