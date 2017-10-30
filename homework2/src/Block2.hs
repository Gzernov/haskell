module Block2
  (
    bin,
    combinations,
    permutations
  ) where

import           Control.Monad (guard)

bin :: Int -> [[Int]]
bin 0 = [[]]
bin x = bin (x - 1) >>= (\elem -> [0 : elem, 1 : elem])

combinations :: Int -> Int -> [[Int]]
combinations n 1 = map (: []) [1 .. n]
combinations n k = combinations n (k - 1) >>= (\elem ->
    filter (not . null)
      (map
        (\x ->
         if x < head elem
           then x:elem
           else [])
  [1 .. n - 1]))

permutations :: [Int] -> [[Int]]
permutations [x] = [[x]]
permutations list@(x:xs) = permutations xs >>= (\elem -> insert [] elem [])
  where
    insert :: [Int] -> [Int] -> [[Int]] -> [[Int]]
    insert _head []    cur = (_head ++ [x]) : cur
    insert _head _tail cur = insert (_head ++ [head _tail]) (tail _tail)
      ((_head ++ x : _tail) : cur)
