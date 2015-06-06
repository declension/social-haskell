module Main where

import Test.Hspec
import App

spec :: Spec
spec = describe "tests" $
    it "obvious" $
        "foo" ++ "bar" `shouldBe` "foobar"

main :: IO ()
main = hspec spec
