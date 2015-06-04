module Main where

import           System.IO
import           Data.Maybe
import qualified Data.Foldable as Foldable (mapM_)
import           Control.Monad (unless)
import qualified Text.Parsec as P
-- The error message infix operator
import Text.Parsec ((<?>))
import Control.Applicative --((<$>), (<$), (<*), (<*>), (*>), (<|>))
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

data Command = Post User String | Read User | Wall User | Follow User User | Invalid String | Exit
    deriving (Show, Eq)

type Output = Maybe String

class Run a where
    run :: a -> Output

instance Run Command where
    run c = Just ("Processing command: " ++ show c)

users :: [User]
users = [newUser{ name="Alice" }]
--users = []

usernameParser :: P.Parsec String () String
usernameParser = P.many1 P.alphaNum

findUserParser :: P.Parsec String () User
findUserParser = do
        userName <- P.choice [P.string (name u) | u <- users]
        --return (fromMaybe (P.unexpected "unknown user") $ find (\u -> name u == userName) users)
        return $ fromJust $ find (\u -> name u == userName) users

wallParser :: P.Parsec String () Command
wallParser = do
    --let user = newUser { name="foo" }
    user <- findUserParser
    P.skipMany1 P.space
    P.string "wall"
    return $ Wall user

readParser :: P.Parsec String () Command
readParser = Read <$> findUserParser

cmd :: Command
cmd = Wall newUser{ name="foo" }

exitParser :: P.Parsec String () Command
exitParser = Exit <$ P.try (P.string "exit" <|> P.string "quit")

redirect :: IO ()
redirect = do
    putStr "> "
    input <- getLine
    let result = parse (P.try readParser <|> wallParser <|> exitParser <* (P.skipMany1 P.space <|> P.eof)) input
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
