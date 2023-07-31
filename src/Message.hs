module Message (Message(..), ClientName) where
import Data.Text (Text)

data Message
    = Notice Text
    | Login ClientName
    | Logout
    | Tell ClientName Text
    | TellTo Text
    | Broadcast ClientName Text
    | BroadcastTo Text

type ClientName = Text

