module Main where

import           System.IO
import           Data.Maybe
import qualified Data.Foldable as Foldable (mapM_)
import           Data.List (find, delete)
import           Data.Functor.Identity (runIdentity)

import           Control.Applicative
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State (State, get, put, runState)
import qualified Text.Parsec as P

-- Get the Identity monad from here:
import Control.Monad.Identity()

data User = User {
          name       :: String
        , posts      :: [String]
        , following  :: [User]
        } deriving (Eq, Ord, Show)
newUser = User {name=undefined, posts=[], following=[]}

data Command = Post User String | Read User | Wall User
               | Follow User User | Debug | Exit
    deriving (Show, Eq)

type Output = Maybe String
type AppState = [User]

run :: Command -> State AppState Output
run (Wall u)     = return $ Just $ ("WALL: for " ++ show u) ++ show (posts u)

run (Read u)     = return $ Just $ show $ posts u
run (Post u s)   = do
    users <- get
    put $ newUser{name = name u, posts = s : posts u} : delete u users
    return Nothing
run Debug        = do
    usrs <- get
    return $ Just $ "Users: " ++ show usrs
run c            = return $ Just ("Processing command: " ++ show c)


-- Convenience definition, that also reads nicely
--type ParserTo = P.ParsecT String () (State AppState)
type ParserTo = P.Parsec String AppState

-- Convenience definitions
usernameParser :: ParserTo String
usernameParser = P.many1 P.alphaNum
whitespace = P.skipMany1 P.space

-- Finds a valid, existing user (only!) by looking up the parsed name
findUserParser :: ParserTo User
findUserParser = do
        users <- P.getState
        username <- P.choice [P.string (name u) | u <- users]
        return $ fromJust $ find (\u -> name u == username) users

-- Finds or creates a user based on parsed username.
findOrCreateUserParser :: ParserTo User
findOrCreateUserParser = do
        users <- P.getState
        username <- usernameParser
        let existing = find (\u -> name u == username) users
        return $ fromMaybe newUser{name=username} existing

wallParser :: ParserTo Command
wallParser = Wall <$> findUserParser <* whitespace <* P.string "wall"

readParser :: ParserTo Command
readParser = Read <$> findUserParser

debugParser :: ParserTo Command
debugParser = Debug <$ P.string "debug"

postParser :: ParserTo Command
postParser = do
    user <- findOrCreateUserParser
    whitespace >> P.string "->" >> whitespace
    message <- P.many1 P.anyToken
    return $ Post user message

exitParser :: ParserTo Command
exitParser = Exit <$ (P.string "exit" <|> P.string "quit")

commandParser :: ParserTo Command
commandParser = P.choice $ map P.try [exitParser, postParser, wallParser, readParser, debugParser]

process :: [User] -> IO ()
process users = do
    putStr "> "
    input <- liftIO getLine
    let result = runIdentity $ P.runParserT (commandParser <* P.spaces <* P.eof) users "" input
    case result of
        Right command -> do
            let (output, st') = runState (run command) users
            Foldable.mapM_ putStrLn output
            unless (command == Exit) $ process st'
        Left err -> do
            liftIO $ putStrLn ("error: " ++ show err)
            process users

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    process []
