module Main where

import Test.Hspec
import App
import Domain
import           Data.Time (getCurrentTime)

alice = newUser{name="Alice"}

testMessageAt ts = Message{text="Hello, world!", timestamp=ts}

spec :: Spec
spec = describe "tests" $ do
    it "show for Users should contain details" $ do
        show alice `shouldContain` "Alice with 0 post"

    it "show for Messages should contain text" $ do
        ts <- getCurrentTime
        show (testMessageAt ts) `shouldContain` "Hello, world!"

    it "formatPost is correct" $ do
        ts <- getCurrentTime
        formatPost ts (testMessageAt ts) `shouldContain` "Hello, world! (0s ago)"

    it "formatWall is correct" $ do
        ts <- getCurrentTime
        formatWall ts (alice, testMessageAt ts) `shouldContain` "Alice - Hello, world! (0s ago)"

--     it "post should return nothing" $ do
--         liftIO $ run (Post alice "foo bar")

main :: IO ()
main = hspec spec
