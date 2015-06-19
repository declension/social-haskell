module Main where

import           Test.Hspec
import           Data.Time (getCurrentTime, addUTCTime, UTCTime)

import           Domain
import           App

alice = newUser{name="Alice", uid=UserId "1234"}

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

    it "Post integration test" $ do
        now <- getCurrentTime
        let (out, Just ms) = process (emptyState now) "Alice -> I love the weather today"
        out `shouldBe` Nothing
        getStateUsers ms `shouldBe` [User{name="Alice",
                                          posts=[Message {text="I love the weather today", timestamp=now}],
                                          uid=UserId "Alice"}]

    it "Following and wall integration test" $ do
        now <- getCurrentTime
        let (_, Just (AppState _ u f)) = process (emptyState now) "Alice -> I love the weather today"
        let (_, Just (AppState _ u2 f2)) = process st "Charlie -> I'm in New York today! Anyone want to have a coffee?"
                                           where st = AppState ((60 * 5) `addUTCTime` now) u f

        let newTime = (60 * 5 + 2) `addUTCTime` now
        let (_, Just (AppState _ u3 f3)) = process st "Charlie follows Alice"
                                           where st = AppState newTime u2 f2
        let (Just out, _) = process st "Charlie wall"
                            where st = AppState newTime u3 f3
        out `shouldContain` "Alice - I love the weather today (5 minutes ago)"
        out `shouldContain` "Charlie - I'm in New York today! Anyone want to have a coffee? (2 seconds ago)"

main :: IO ()
main = hspec spec
