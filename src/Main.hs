module Main where

import           System.IO
import           Data.Maybe
import qualified Data.Foldable as Foldable (mapM_)
import           Control.Monad (unless)
import qualified Text.Parsec as P
-- The error message infix operator
import Text.Parsec ((<?>))
import Control.Applicative
import Data.List (find)

-- Get the Identity monad from here:
import Control.Monad.Identity()

-- alias Parsec.parse for more concise usage:
parse rule = P.parse rule "(input)"

data User = User {
          name       :: String
        , posts      :: [String]
        , following  :: [User]
        } deriving (Eq, Ord, Show)
newUser = User {name=undefined, posts=[], following=[]}

data Command = Post User String | Read User | Wall User
               | Follow User User | Exit
    deriving (Show, Eq)

type Output = Maybe String

class Run a where
    run :: a -> Output

instance Run Command where
    run (Wall u)     = Just $ ("WALL: for " ++ show u) ++ show (posts u)
    run (Read u)     = Just $ show $ posts u
    run (Post u s)   = Just $ show $ s : posts u
    run c            = Just ("Processing command: " ++ show c)

users :: [User]
users = [newUser{ name="Alice" }]
--users = []

-- Convenience definitions
usernameParser :: P.Parsec String () String
usernameParser = P.many1 P.alphaNum
whitespace = P.skipMany1 P.space

-- Finds a valid, existing user (only!) by looking up the parsed name
findUserParser :: P.Parsec String () User
findUserParser = do
        username <- P.choice [P.string (name u) | u <- users]
        return $ fromJust $ find (\u -> name u == username) users

-- Finds or creates a user based on parsed username.
findOrCreateUserParser :: P.Parsec String () User
findOrCreateUserParser = do
        username <- usernameParser
        let existing = find (\u -> name u == username) users
        return $ fromMaybe newUser{name=username} existing

wallParser :: P.Parsec String () Command
wallParser = Wall <$> findUserParser <* whitespace <* P.string "wall"

readParser :: P.Parsec String () Command
readParser = Read <$> findUserParser

postParser :: P.Parsec String () Command
postParser = do
    user <- findOrCreateUserParser
    whitespace >> P.string "->" >> whitespace
    message <- P.many1 P.anyToken
    return $ Post user message

exitParser :: P.Parsec String () Command
exitParser = Exit <$ (P.string "exit" <|> P.string "quit")

commandParser = P.choice $ map P.try [exitParser, postParser, wallParser, readParser]

redirect :: IO ()
redirect = do
    putStr "> "
    input <- getLine
    let result = parse (commandParser <* P.spaces <* P.eof) input
    case result of
        Right command -> do
            let output = run command
            Foldable.mapM_ putStrLn output
            unless (command == Exit) redirect
        Left err -> do
            putStrLn ("error: " ++ show err)
            redirect

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    redirect
