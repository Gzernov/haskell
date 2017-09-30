module Homework_1
    (
    ) where
import           Data.Bits
import           Data.List
import           System.Random

--Block 2 Task 4
randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

-- Block 1
order3 :: (Int, Int, Int) -> (Int, Int, Int)
order3 (x, y, z) = let [a,b,c] = sort [x,y,z] in (c, b, a)

highestBit :: Int -> Int
highestBit = fst . highestBitExtended

highestBitExtended :: Int -> (Int, Int)
highestBitExtended x = let z = max x 0 in let y = max (finiteBitSize z - countLeadingZeros z - 1) 0 in (2 ^ y, y)

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
mergeSort y@(x:xs) =
  case y of
    [f, s] -> [min f s, max f s]
    [f] -> [x]
    lst -> let (first, second) = splitList y
      in sortLists (mergeSort first) (mergeSort second) []
      where
        sortLists :: Ord a => [a] -> [a] -> [a] -> [a]
        sortLists x@(xh:xt) y@(yh:yt) res =
          if x < y
          then
            sortLists xt y (res ++ [xh])
          else
            sortLists x yt (res ++ [yh])
        sortLists [] y res = res ++ y
        sortLists x [] res = res ++ x

        splitList :: [a] -> ([a], [a])
        splitList x = splitAt (length x `div` 2) x
