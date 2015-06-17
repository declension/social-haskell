module Main where

import Test.Hspec
import Domain
import           Data.Time (getCurrentTime, addUTCTime, UTCTime)

alice = newUser{name="Alice"}

testMessageAt :: UTCTime -> Message
testMessageAt ts = Message{text="Hello, world!", timestamp=ts}

spec :: Spec
spec = describe "tests" $ do
    it "show for Users should contain details" $
        show alice `shouldContain` "Alice with 0 post"

    it "show for Messages should contain text" $ do
        ts <- getCurrentTime
        show (testMessageAt ts) `shouldContain` "Hello, world!"

    it "formatPost is correct" $ do
        ts <- getCurrentTime
        formatPost ts (testMessageAt ts) `shouldContain` "Hello, world! (just now)"

    it "formatWall is correct" $ do
        ts <- getCurrentTime
        formatWall ts (alice, testMessageAt ts) `shouldContain` "Alice - Hello, world! (just now)"

    it "ago should work for a few seconds" $ do
        now <- getCurrentTime
        let earlier = 5 `addUTCTime` now
        ago earlier now `shouldBe` "5 seconds ago"

    it "ago should work a second" $ do
        now <- getCurrentTime
        let earlier = 1 `addUTCTime` now
        ago earlier now `shouldBe` "1 second ago"

    it "ago should work singular minutes" $ do
        now <- getCurrentTime
        let earlier = 61 `addUTCTime` now
        ago earlier now `shouldBe` "1 minute ago"

    it "ago should work for hours" $ do
        now <- getCurrentTime
        let earlier = (60 * 60 * 2) `addUTCTime` now
        ago earlier now `shouldBe` "2 hours ago"

    it "ago should work for days" $ do
        now <- getCurrentTime
        let earlier = (60 * 60 * 24 * 3) `addUTCTime` now
        ago earlier now `shouldBe` "3 days ago"


main :: IO ()
main = hspec spec
