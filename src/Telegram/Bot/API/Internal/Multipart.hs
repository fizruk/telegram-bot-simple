module Telegram.Bot.API.Internal.Multipart where
import Servant.Multipart (FileData (FileData), Tmp)
import Telegram.Bot.API.Types
import qualified Data.Text as T
import System.FilePath


-- | Multipart file helper, works only with InputFile constructor. 
makeFile :: T.Text -> InputFile -> FileData Tmp
makeFile name (InputFile path ct) = FileData name (T.pack $ takeFileName path) ct path
makeFile _ _ = error "Bad input file for multipart"
