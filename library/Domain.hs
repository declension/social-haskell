module Domain where
import           Text.Printf (printf)
import           Data.Time (diffUTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.Time.Clock (UTCTime)

data Command = Post User String | Read User | Wall User
               | Follow User User | Debug | Exit
    deriving (Show, Eq)

type Output = Maybe String
-- Pass around state both in our parser (kind of) and in the execution of commands
type AppState = (UTCTime, [User])


data Message = Message { text :: String, timestamp :: UTCTime}
    deriving (Eq, Ord)

instance Show Message where
    show m = printf "\"%s\" @ %s" (text m) (formatTime defaultTimeLocale "%H:%M:%S on %Y-%m-%d " $ timestamp m)

data User = User {
          name       :: String
        , posts      :: [Message]
        , following  :: [User]
        } deriving (Eq, Ord)
newUser = User {name=undefined, posts=[], following=[]}

instance Show User where
    show u = printf "<%s with %d post(s), following:%s>" (name u) (length $ posts u) (show $ following u)


-- Pretty prints time deltas
ago:: UTCTime -> Message -> String
ago now m = show $ diffUTCTime now $ timestamp m

formatWall :: UTCTime -> (User, Message) -> String
formatWall now (u, m) = printf "%s - %s (%s ago)" (name u) (text m) (ago now m)

