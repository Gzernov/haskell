{-# LANGUAGE ExplicitForAll #-}

module Homework_1_tests
    (
    ) where
import           Homework_1

data TestResult a b = Failed a b b | Success a b
  deriving Show

perfomTestSimple :: Eq b => (a -> b) -> a -> b -> TestResult a b
perfomTestSimple func inp expected =
  let
    got = func inp
    in
      if got == expected
        then Success inp expected
        else Failed inp expected got

perfomTestTwoArgs :: Eq c => (a -> b -> c) -> a -> b -> c -> TestResult (a, b) c
perfomTestTwoArgs func arg sarg expected =
  let
    got = func arg sarg
    in
      if got == expected
        then Success (arg, sarg) expected
        else Failed (arg, sarg) expected got

testOrder3 :: TestResult (Int, Int, Int) (Int, Int, Int)
testOrder3 = perfomTestSimple order3 (5, 2, 6) (2, 5, 6)

testHighestBit :: [TestResult Int Int]
testHighestBit = [perfomTestSimple highestBit 15 8,
  perfomTestSimple highestBit 16 16, perfomTestSimple highestBit 17 16]

testHighestBitExtended :: [TestResult Int (Int, Int)]
testHighestBitExtended = [perfomTestSimple highestBitExtended 15 (8, 3),
  perfomTestSimple highestBitExtended 16 (16, 4), perfomTestSimple highestBitExtended 17 (16, 4)]

testSmartReplicate :: TestResult [Int] [Int]
testSmartReplicate = perfomTestSimple smartReplicate [1,2,3] [1,2,2,3,3,3]

testContains :: TestResult (Int, [[Int]]) [[Int]]
testContains = perfomTestTwoArgs contains 3 [[1..5], [2,0], [3,4]] [[1,2,3,4,5],[3,4]]

testRemoveAt :: [TestResult(Int, [Integer]) [Integer]]
testRemoveAt = [perfomTestTwoArgs removeAt 1 [1,2,3] [1,3],
  perfomTestTwoArgs removeAt 10 [1,2,3] [1,2,3],
  perfomTestTwoArgs removeAt 3 [1..5] [1,2,3,5]]

testRemoveAtExtended :: [TestResult (Int, [Integer]) (Maybe Integer, [Integer])]
testRemoveAtExtended = [perfomTestTwoArgs removeAtExtended 1 [1,2,3] (Just 2, [1,3]),
  perfomTestTwoArgs removeAtExtended 10 [1,2,3] (Nothing, [1,2,3]),
  perfomTestTwoArgs removeAtExtended 3 [1..5] (Just 4, [1,2,3,5])]

testCollectEvery :: TestResult (Int, [Int]) ([Int], [Int])
testCollectEvery = perfomTestTwoArgs collectEvery 3 [1..8] ([1,2,4,5,7,8], [3,6])

testStringSum :: [TestResult String Integer]
testStringSum = [perfomTestSimple stringSum "1 1" 2,
  perfomTestSimple stringSum "100\n\t-3" 97,
  perfomTestSimple stringSum "\t-12345\t" (-12345),
  perfomTestSimple stringSum "123\t\n\t\n\t\n321 -4 -40" 400,
  perfomTestSimple stringSum "+1 -1" 0]

testMergeSort :: TestResult [Int] [Int]
testMergeSort = perfomTestSimple mergeSort [2, 1, 0, 3, 10, 5] [0, 1, 2, 3, 5, 10]

--third block
testNextDay :: TestResult Day Day
testNextDay = perfomTestSimple nextDay Sun Mon

testAfterDays :: TestResult (Day, Int) Day
testAfterDays = perfomTestTwoArgs afterDays Sun 6 Sat

testIsWeekend :: [TestResult Day Bool]
testIsWeekend = [perfomTestSimple isWeekend Sun True,
  perfomTestSimple isWeekend Mon False]

testDaysToParty :: TestResult Day Int
testDaysToParty = perfomTestSimple daysToParty Sat 6


testFight :: TestResult (Monster, Knight) (Either Monster Knight, RestHp)
testFight = let
  monster = Monster (Hp 3) (Attack 2)
  knight = Knight (Hp 2) (Attack 2)
  result = Monster (Hp 1) (Attack 2)
  restHp = RestHp (Hp 1)
  in perfomTestTwoArgs fight monster knight (Left result, restHp)
