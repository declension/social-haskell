module Main where

import Text.Printf (printf)
import Data.Maybe
import qualified Data.Foldable as Foldable (mapM_)
--import Data.Either.Unwrap
import Control.Monad (when, unless)
import System.IO

-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as P

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity()

-- alias Parsec.parse for more concise usage in my examples:
parse rule = P.parse rule "(source)"

data User = User {
          getName       :: String
        , getPosts      :: [String]
        , getFollowing  :: [User]
        } deriving (Eq, Ord, Show)

data Command = Post String | Read | Wall String | Follow User | Exit
    deriving (Show, Eq)

type Output = Maybe String

runCmd :: Command -> Output
runCmd c = Just ("Processing command: " ++ show c)


usernameParser :: P.Parsec String () String
usernameParser = P.many1 P.alphaNum

wallParser :: P.Parsec String () Command
wallParser = Wall <$> usernameParser <* P.spaces <* P.string "wall"

exitParser :: P.Parsec String () Command
exitParser = Exit <$ P.try (P.string "exit" <|> P.string "quit" <?> "An exit token")


redirect :: IO ()
redirect = do
    input <- getLine
    let result = parse (P.try wallParser <|> exitParser) input
    case result of
        Right command   -> do
            let output = runCmd command
            Foldable.mapM_ putStrLn output
            unless (command == Exit) redirect
        Left  err       -> do
            putStrLn ("error: " ++ show err)
            redirect

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    redirect
