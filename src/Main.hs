module Main where

import           System.IO
--import           Text.Printf (printf)
--import           Data.Maybe
import qualified Data.Foldable as Foldable (mapM_)
import           Control.Monad (unless)

import qualified Text.Parsec as P

-- The error message infix operator
import Text.Parsec ((<?>))

import Control.Applicative ((<$>), (<$), (<*), (<|>))

-- Get the Identity monad from here:
import Control.Monad.Identity()

-- alias Parsec.parse for more concise usage:
parse rule = P.parse rule "(input)"

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
    putStr "> "
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
