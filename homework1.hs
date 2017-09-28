import Data.Bits
import Data.List

-- Block 1
order3 :: (Int, Int, Int) -> (Int, Int, Int)
order3 (x, y, z) = let [a,b,c] = sort [x,y,z] in (c, b, a)

highestBit :: Int -> (Int, Int)
-- handle not positive numbers
highestBit x = let z = max x 0 in let y = max (finiteBitSize z - countLeadingZeros z - 1) 0 in (2 ^ y, y)
-- doesn't handle not positive numbers
-- highestBit x = let y = finiteBitSize x - countLeadingZeros x - 1 in (2 ^ y, y)

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\y -> replicate y y)

contains :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem

-- Block 2
