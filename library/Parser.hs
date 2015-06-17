module Parser where
import           Data.Maybe
import           Data.List (find)
import           Control.Applicative
import qualified Text.Parsec as P
-- Get the Identity monad from here:
import Control.Monad.Identity()

import Domain
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

followParser :: ParserTo Command
followParser = Follow <$> findUserParser
                      <*  whitespace <* P.string "follows" <* whitespace
                      <*> findUserParser

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
commandParser = P.choice $ map P.try [exitParser, followParser, postParser, wallParser, readParser,  debugParser]
