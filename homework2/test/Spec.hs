module Spec
    (
    ) where

import Test.Hspec
import Block1
import Block2
import Block4
import Data.Char ( isUpper )

main :: IO ()
main = hspec $
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
    describe "2nd block" $ do
      it "Bin" $ do
        bin 1 `shouldBe` [[0],[1]]
        bin 2 `shouldBe` [ [ 0 , 0 ] , [ 1 , 0 ] , [ 0 , 1 ] , [ 1 , 1 ] ]
        bin 3 `shouldBe`
          [ [ 0 , 0 , 0 ]
          , [ 1 , 0 , 0 ]
          , [ 0 , 1 , 0 ]
          , [ 1 , 1 , 0 ]
          , [ 0 , 0 , 1 ]
          , [ 1 , 0 , 1 ]
          , [ 0 , 1 , 1 ]
          , [ 1 , 1 , 1 ]
          ]
      it "Combinations" $
        combinations 4 2 `shouldBe`
          [ [1, 2]
          , [1, 3]
          , [2, 3]
          , [1, 4]
          , [2, 4]
          , [3, 4]
          ]
      it "Permutations" $
        permutations [22, 10, 5] `shouldBe`
          [ [5, 10, 22]
          , [5, 22, 10]
          , [22, 5, 10]
          , [10, 5, 22]
          , [10, 22, 5]
          , [22, 10, 5]
          ]

    describe "4th block" $ do
      it "Basics" $ do
        runParser abParser "abcdef" `shouldBe` Just (('a','b'),"cdef")
        runParser abParser "aebcdf" `shouldBe` Nothing

        runParser abParser_ "abcdef" `shouldBe` Just ((),"cdef")
        runParser abParser_ "aebcdf" `shouldBe` Nothing

        runParser intPair "12 34" `shouldBe` Just ([12,34],"")
        runParser intPair "12 34 asdf" `shouldBe` Just ([12,34]," asdf")

        runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
        runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
        runParser intOrUppercase "foo" `shouldBe` Nothing

      it "Simple parser" $ do
        runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
        runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`  Just ("ABC","dEfgH")
        runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Just ("","abcdeFGh")
        runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing

        runParser spaces "      " `shouldBe` Just ("      ", "")

        runParser ident "foobar baz" `shouldBe` Just ("foobar"," baz")
        runParser ident "foo33fA" `shouldBe` Just ("foo33fA","")
        runParser ident "2bad" `shouldBe` Nothing
        runParser ident "" `shouldBe` Nothing

        runParser parseSExpr "5" `shouldBe` Just (A (N 5),"")
        runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"),"")
        runParser parseSExpr "foo3 bar" `shouldBe` Just (A (I "foo3"),"bar")
        runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe`
          Just (Comb [A (I "bar"),A (I "foo"),A (N 3),A (N 5),A (N 874)],"")
        runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe`
          Just (
            Comb
              [Comb
                [Comb
                  [A (I "lambda")
                  ,A (I "x"),
                  Comb
                    [A (I "lambda")
                    ,A (I "y")
                    ,Comb
                      [A (I "plus")
                      ,A (I "x")
                      ,A (I "y")
                      ]
                    ]
                  ]
                  ,A (N 3)
                ]
              ,A (N 5)
            ],"")
        runParser parseSExpr "( lots of ( spaces in ) this ( one ) )" `shouldBe`
          Just (
            Comb
              [A (I "lots")
              ,A (I "of")
              ,Comb
                [A (I "spaces")
                ,A (I "in")
                ]
              ,A (I "this")
              ,A (I "one")
              ],"")
        runParser parseSExpr "((()" `shouldBe` Nothing

        let f = "let x = 1 + 2 + 5"
        let s = "let   y = x+x"
        let t = "let z=0+    x   + y + 8"
        let testStr = f ++ "\n" ++ s ++ "\n" ++ t
        let testStrInvalid = "let a = 1 + 2 + b"
        let testStrInvalid2 = f ++ "\n" ++ testStrInvalid

        parseAndSimplify testStr `shouldBe` Just ["let x = 8","let y = 16","let z = 32"]
        parseAndSimplify testStrInvalid `shouldBe` Nothing
        parseAndSimplify testStrInvalid `shouldBe` Nothing
