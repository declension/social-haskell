module App where
import           Data.List (delete, sortBy)
import           Data.Function (on)
import           Data.Time (getCurrentTime)
import           Control.Monad.Trans.State (State, get, put, runState)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Applicative
import           Data.Functor.Identity (runIdentity)
import qualified Data.Foldable as Foldable (mapM_)
import qualified Text.Parsec as P
import           Control.Monad.Identity()
import           Parser
import           Domain


run :: Command -> State AppState Output

run (Wall user)     = do
    (now, _) <- get
    let users = user : following user
    let sorted = sortBy (flip compare `on` (timestamp . snd)) ps
                 where ps = concatMap (\u -> map (\m -> (u, m)) $ posts u) users
    return $ Just $ unlines $ map (formatWall now) sorted

run (Read u)     = do
    (now, _) <- get
    return $ Just $ unlines $ map (formatPost now) $ posts u

run (Post u s)   = do
    (now, users) <- get
    put $ stateWithUpdatedUser now users u newUser{name = name u, posts = Message s now : posts u}
    return Nothing

run (Follow u followed) = do
    (now, users) <- get
    let new = newUser{name = name u, posts = posts u, following = followed : following u}
    put $ stateWithUpdatedUser now users u new
    return Nothing

run Debug        = do
    usrs <- get
    return $ Just $ "Users: " ++ show usrs

run c            = return $ Just ("Processing command: " ++ show c)

stateWithUpdatedUser :: t -> [User] -> User -> User -> (t, [User])
stateWithUpdatedUser now users u u' = (now, u' : delete u users)


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
