module Main where

import           System.IO
import           Data.Maybe
import qualified Data.Foldable as Foldable (mapM_)
import           Control.Monad (unless,liftM)
import           Control.Monad.Trans.State
import qualified Text.Parsec as P
-- The error message infix operator
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.List (find, delete)

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
type AppState = [User]

run :: Command -> State AppState Output
run (Wall u)     = return $ Just $ ("WALL: for " ++ show u) ++ show (posts u)

run (Read u)     = return $ Just $ show $ posts u
run (Post u s)   = do
    users <- get
    -- Remove user u
    -- add updated user u
    put $ newUser{name = name u, posts = s : posts u} : delete u users
    return $ Just $ show $ s : posts u
run c            = return $ Just ("Processing command: " ++ show c)

users :: [User]
users = [newUser{ name="Alice" }]
--users = []

-- Convenience definition, that also reads nicely
--type ParserTo = P.ParsecT String () (State AppState)
type ParserTo = P.ParsecT String () (State AppState)

-- Convenience definitions
usernameParser :: ParserTo String
usernameParser = P.many1 P.alphaNum
whitespace = P.skipMany1 P.space

-- Finds a valid, existing user (only!) by looking up the parsed name
findUserParser :: ParserTo User
findUserParser = do
        username <- P.choice [P.string (name u) | u <- users]
        return $ fromJust $ find (\u -> name u == username) users

-- Finds or creates a user based on parsed username.
findOrCreateUserParser :: ParserTo User
findOrCreateUserParser = do
        username <- usernameParser
        let existing = find (\u -> name u == username) users
        return $ fromMaybe newUser{name=username} existing

wallParser :: ParserTo Command
wallParser = Wall <$> findUserParser <* whitespace <* P.string "wall"

readParser :: ParserTo Command
readParser = Read <$> findUserParser

postParser :: ParserTo Command
postParser = do
    user <- findOrCreateUserParser
    whitespace >> P.string "->" >> whitespace
    message <- P.many1 P.anyToken
    return $ Post user message

exitParser :: ParserTo Command
exitParser = Exit <$ (P.string "exit" <|> P.string "quit")

commandParser :: ParserTo Command
commandParser = P.choice $ map P.try [exitParser, postParser, wallParser, readParser]

process :: [User] -> IO ()
process users = do
    putStr "> "
    input <- liftIO getLine
    let (result, st) = runState (P.runParserT (commandParser <* P.spaces <* P.eof) () "" input) users
    case result of
        Right command -> do
            let (output, st') = runState (run command) st
            Foldable.mapM_ putStrLn output
            unless (command == Exit) $ process st'
        Left err -> do
            liftIO $ putStrLn ("error: " ++ show err)
            process st

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    --runState redirect $ liftIO users
    process users
    return ()
