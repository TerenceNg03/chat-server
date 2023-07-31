module Message (Message(..), ClientName) where
import Data.Text (Text)

data Message
    = Notice Text
    | Login ClientName
    | Logout
    | Tell {from :: ClientName , to :: ClientName, msg :: Text}
    | TellTo ClientName Text
    | Broadcast ClientName Text
    | BroadcastTo Text
    | Kick ClientName Text
    | Disconnect

type ClientName = Text

