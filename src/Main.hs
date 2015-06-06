module Main where

import           System.IO
import           Data.Maybe
import qualified Data.Foldable as Foldable (mapM_)
import           Data.List (find, delete)
import           Data.Functor.Identity (runIdentity)
import           Data.Time.Clock (UTCTime)
import           Data.Time (getCurrentTime, diffUTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)

import           Control.Applicative
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State (State, get, put, runState)
import qualified Text.Parsec as P
import           Text.Printf (printf)

-- Get the Identity monad from here:
import Control.Monad.Identity()

data Message = Message { text :: String, timestamp :: UTCTime}
    deriving (Eq, Ord)
instance Show Message where
    show m = printf "\"%s\" @ %s" (text m) (show $ formatTime defaultTimeLocale "%H:%M:%S on %Y-%m-%d " $ timestamp m)


data User = User {
          name       :: String
        , posts      :: [Message]
        , following  :: [User]
        } deriving (Eq, Ord)
newUser = User {name=undefined, posts=[], following=[]}

instance Show User where
    show u = printf "<%s with %d post(s), following:%s>" (name u) (length $ posts u) (show $ following u)

data Command = Post User String | Read User | Wall User
               | Follow User User | Debug | Exit
    deriving (Show, Eq)

type Output = Maybe String
-- Pass around state both in our parser (kind of) and in the execution of commands
type AppState = (UTCTime, [User])


-- Pretty prints time deltas
ago:: UTCTime -> Message -> String
ago now m = show $ diffUTCTime now $ timestamp m

formatWall :: UTCTime -> (User, Message) -> String
formatWall now (u, m) = printf "%s - %s (%s ago)" (name u) (text m) (ago now m)

run :: Command -> State AppState Output
run (Wall user)     = do
    (now, _) <- get
    let users = user : following user
    let ps = concatMap (\u -> map (\m -> (u, m)) $ posts u) users
    return $ Just $ unlines $ map (formatWall now) ps

run (Read u)     = return $ Just $ show $ posts u
run (Post u s)   = do
    (now, users) <- get
    put (now, newUser{name = name u, posts = Message s now : posts u} : delete u users)
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
        (_, users) <- P.getState
        username <- P.choice [P.string (name u) | u <- users]
        return $ fromJust $ find (\u -> name u == username) users

-- Finds or creates a user based on parsed username.
findOrCreateUserParser :: ParserTo User
findOrCreateUserParser = do
        (_,users) <- P.getState
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

-- The uber-parser
commandParser :: ParserTo Command
commandParser = P.choice $ map P.try [exitParser, postParser, wallParser, readParser, debugParser]

-- Main loop, taking (initial) state and running commands over this
process :: [User] -> IO ()
process users = do
    putStr "> "
    input <- liftIO getLine
    now <- getCurrentTime
    let result = runIdentity $ P.runParserT (commandParser <* P.spaces <* P.eof) (now, users) "" input
    case result of
        Right command -> do
            let (output, (_, users')) = runState (run command) (now, users)
            Foldable.mapM_ putStrLn output
            unless (command == Exit) $ process users'
        Left err -> do
            liftIO $ putStrLn ("error: " ++ show err)
            process users

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    process []
