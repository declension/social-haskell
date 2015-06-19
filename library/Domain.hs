module Domain where
import           Text.Printf (printf)
import           Data.Time (diffUTCTime, NominalDiffTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.Time.Clock (UTCTime)
import           Data.Map (Map, fromList)
import           Data.List (find)

data Command = Post User String | Read User | Wall User
               | Follow User User | Debug | Exit
    deriving (Show, Eq)

type Output = Maybe String

data Message = Message { text :: String, timestamp :: UTCTime}
    deriving (Eq, Ord)

instance Show Message where
    show m = printf "\"%s\" @ %s" (text m) (formatTime defaultTimeLocale "%H:%M:%S on %Y-%m-%d " $ timestamp m)

newtype UserId = UserId String deriving (Show, Eq, Ord)

data User = User {
          uid        :: UserId
        , name       :: String
        , posts      :: [Message]
        } deriving (Eq, Ord)
newUser = User {name=undefined, uid=undefined, posts=[]}

-- Allow following
type Users = [User]
type Following = Map UserId [UserId]

instance Show User where
    show u = printf "<%s with %d post(s)>" (name u) (length $ posts u)

-- Pass around state both in our parser (kind of) and in the execution of commands
data AppState = AppState {getStateTime :: UTCTime, getStateUsers :: Users, getStateFollowing :: Following}

userById :: Users -> UserId -> Maybe User
userById users x = find ((==x) . uid) users


-- Pretty prints time deltas
ago :: UTCTime -> UTCTime -> String
--ago now m = show $ diffUTCTime now $ timestamp m
ago now ts
    | secs  < 1 = "just now"
    | mins  < 1 = numFor secs "second"
    | hours < 1 = numFor mins "minute"
    | days  < 1 = numFor hours "hour"
    | otherwise = numFor days "day"
    where secs   = realToFrac $ diffUTCTime now ts
          mins   = secs / 60
          hours  = mins / 60
          days   = hours / 24

numFor :: NominalDiffTime -> String -> String
numFor n s = printf "%d %s%s ago" intN s (if intN == 1 then "" else "s")
             where intN = round (realToFrac n) :: Integer

formatWall :: UTCTime -> (User, Message) -> String
formatWall now (u, m) = printf "%s - %s (%s)" (name u) (text m) (ago now $ timestamp m)

formatPost :: UTCTime -> Message -> String
formatPost now m = printf "%s (%s)" (text m) (ago now $ timestamp m)
