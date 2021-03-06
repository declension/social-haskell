module App where
import           Data.List (delete, sortBy, intercalate)
import           Data.Function (on)
import           Data.Time (getCurrentTime)
import           Data.Time.Clock (UTCTime)
import           Data.Map (findWithDefault, fromList, toList, empty, insertWith)
import           Data.Maybe (fromJust)
import           Control.Monad.Trans.State (State, get, put, runState)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Applicative ((<*))
import           Data.Functor.Identity (runIdentity)
import qualified Data.Foldable as Foldable (mapM_)
import qualified Text.Parsec as P
import           Control.Monad.Identity()
import           Parser
import           Domain
import           Text.Printf (printf)

emptyState :: UTCTime -> AppState
emptyState ts = AppState ts [] fol
                where fol = empty :: Following

run :: Command -> State AppState Output

run (Wall user)     = do
    AppState now users fols <- get
    let allUsers = user : map (fromJust . userById users) followUserIds
                   where followUserIds = findWithDefault [] (uid user) fols
    let sorted = sortBy (flip compare `on` (timestamp . snd)) ps
                 where ps = concatMap (\u -> map (\m -> (u, m)) $ posts u) allUsers
    return $ Just $ unlines $ map (formatWall now) sorted

run (Read u)     = do
    AppState now _  _ <- get
    return $ Just $ unlines $ map (formatPost now) $ posts u

run (Post u s)   = do
    AppState now users f <- get
    put $ stateWithUpdatedUser now users u newUser{name = name u, uid = uid u, posts = Message s now : posts u} f
    return Nothing

run (Follow u followed) = do
    AppState now users fols <- get
    let folId = uid followed
    -- Update the map in-place: append the ID of the new followed user or create a singelton list if first for that user
    put $ AppState now users (insertWith (++) (uid u) [folId] fols)
    return Nothing

run Debug        = do
    AppState _ users fols <- get
    let prettyFols = map (\(UserId uid', folIds) -> uid' ++ " -> " ++ show folIds) $ toList fols
    return $ Just $ printf "Users    : %s\nFollowing: %s" (show users) $ intercalate ", " prettyFols


run c            = return $ Just ("Processing command: " ++ show c)

stateWithUpdatedUser :: UTCTime -> Users -> User -> User -> Following -> AppState
stateWithUpdatedUser now users u u' = AppState now (u' : delete u users)


-- Takes a command, parses and runs it to return an output and updated state (IO-free)
process :: AppState -> String -> (Output, Maybe AppState)
process state input = do
    let result = runIdentity $ P.runParserT (commandParser <* P.spaces <* P.eof) state "" input
    case result of
        Right Exit -> (Just "Goodbye!", Nothing)
        Right command -> (out, Just state')
                         where (out, state') = runState (run command) state
        Left err -> (Just $ "error: " ++ show err, Just state)

-- Main loop, taking (initial) state and running commands over this
loop :: AppState -> IO ()
loop (AppState _ u f) = do
    putStr "> "
    input <- liftIO getLine
    now <- getCurrentTime
    let (output, maybeState) = process (AppState now u f) input
    Foldable.mapM_ putStrLn output
    Foldable.mapM_ loop maybeState
    return ()
