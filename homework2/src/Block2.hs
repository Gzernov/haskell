module Block2
  (
  ) where

import           Control.Monad      (guard)

bin :: Int -> [[Int]]
bin 0 = [[]]
bin x = bin (x - 1) >>= (\elem -> [0 : elem, 1 : elem])

combination :: Int -> Int -> [[Int]]
combination n 1 = map (\x -> [x]) [1 .. n]
combination n k = (combination n (k - 1)) >>= (\elem ->
    filter (\x -> not $ null x)
      (map
        (\x ->
         if x < head(elem)
           then x:elem
           else [])
  [1 .. n - 1]))

permutations :: [Int] -> [[Int]]
permutations [x] = [[x]]
permutations [x, y] = [[x, y], [y, x]]
permutations list@(x:xs) = permutations(xs) >>= (\elem -> map (:elem) list)
