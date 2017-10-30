module Spec
    (
    ) where

import Test.Hspec
import Block1

main :: IO ()
main = hspec $ do
  describe "2nd homework" $ do
    describe "1st block" $ do
      let one = Const 1
      let zero = Const 0
      let mone = Const (-1)
      let two = Const 2
      it "Valid" $ do
        eval one `shouldBe` Right 1
        eval (Sum two two) `shouldBe` Right 4
        eval (Sub (Sum two two) one) `shouldBe` Right 3
        eval (Mul one two) `shouldBe` Right 2
        eval (Div (Mul two two) two) `shouldBe` Right 2
        eval (Pow (Sum two one) two) `shouldBe` Right 9
      it "Invalid" $ do
        eval (Div one zero) `shouldBe` Left (ArithmeticError "Zero division")
        eval (Pow two mone) `shouldBe` Left (ArithmeticError "Negative pow")
