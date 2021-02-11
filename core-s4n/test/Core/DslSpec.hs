{-# LANGUAGE OverloadedStrings #-}

module Core.DslSpec where

import Core.Dsl
import Data.Either (fromRight)
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldBe,
    shouldThrow,
  )

spec :: Spec
spec = do
  describe "Dsl" $ do
    context "makeCmd AAAAIAA" $ do
      it "Should return a A(A(A(A(I(A(A()))))))" $ do
        let cmd = "AAAAIAA"
            result = fromRight "" (show <$> makeCmd cmd)
            expected = "A(A(A(A(I(A(A()))))))"
        result `shouldBe` expected
    context "eval AAAAIAA" $ do
      it "Should return a (-2, 4) East" $ do
        let cmd = "AAAAIAA"
            result = fromRight "" (show . eval <$> makeCmd cmd)
            expected = "(-2, 4) East"
        result `shouldBe` expected
    context "makeCmd AADADA" $ do
      it "Should return a A(A(A(A(I(A(A()))))))" $ do
        let cmd = "AADADA"
            result = fromRight "" (show <$> makeCmd cmd)
            expected = "A(A(D(A(D(A())))))"
        result `shouldBe` expected
    context "eval AADADA" $ do
      it "Should return a (1, 1) South" $ do
        let cmd = "AADADA"
            result = fromRight "" (show . eval <$> makeCmd cmd)
            expected = "(1, 1) South"
        result `shouldBe` expected
    context "makeCmd DDDAIAD" $ do
      it "Should return a A(A(A(A(I(A(A()))))))" $ do
        let cmd = "DDDAIAD"
            result = fromRight "" (show <$> makeCmd cmd)
            expected = "D(D(D(A(I(A(D()))))))"
        result `shouldBe` expected
    context "eval DDDAIAD" $ do
      it "Should return a (-1, -1) East" $ do
        let cmd = "DDDAIAD"
            result = fromRight "" (show . eval <$> (makeCmd cmd))
            expected = "(-1, -1) East"
        result `shouldBe` expected
    context "makeCmd AAIADAD" $ do
      it "Should return a A(A(I(A(D(A(D()))))))" $ do
        let cmd = "AAIADAD"
            result = fromRight "" (show <$> makeCmd cmd)
            expected = "A(A(I(A(D(A(D()))))))"
        result `shouldBe` expected
    context "eval AAIADAD" $ do
      it "Should return a (-1, 3) West" $ do
        let cmd = "AAIADAD"
            result = fromRight "" (show . eval <$> makeCmd cmd)
            expected = "(-1, 3) West"
        result `shouldBe` expected
