{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.AnswerCallbackQuery where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'answerCallbackQuery'

-- | Request parameters for 'answerCallbackQuery'.
data AnswerCallbackQueryRequest = AnswerCallbackQueryRequest
  { answerCallbackQueryCallbackQueryId :: CallbackQueryId -- ^ Unique identifier for the query to be answered
  , answerCallbackQueryText :: Maybe Text -- ^ Text of the notification. If not specified, nothing will be shown to the user, 0-200 characters
  , answerCallbackQueryShowAlert :: Maybe Bool -- ^ If True, an alert will be shown by the client instead of a notification at the top of the chat screen. Defaults to false.
  , answerCallbackQueryUrl :: Maybe Text
    -- ^ URL that will be opened by the user's client. If you have created a Game and accepted the conditions via @Botfather, specify the URL that opens your game — note that this will only work if the query comes from a callback_game button.
    --
    --   Otherwise, you may use links like t.me/your_bot?start=XXXX that open your bot with a parameter.
  , answerCallbackQueryCacheTime :: Maybe Integer -- ^ The maximum amount of time in seconds that the result of the callback query may be cached client-side. Telegram apps will support caching starting in version 3.14. Defaults to 0.
  }
  deriving Generic

instance ToJSON   AnswerCallbackQueryRequest where toJSON = gtoJSON
instance FromJSON AnswerCallbackQueryRequest where parseJSON = gparseJSON

type AnswerCallbackQuery = "answerCallbackQuery"
  :> ReqBody '[JSON] AnswerCallbackQueryRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to send answers to callback 
--   queries sent from inline keyboards. The answer 
--   will be displayed to the user as a notification 
--   at the top of the chat screen or as an alert. 
--   On success, True is returned.
--
--  Alternatively, the user can be redirected to 
--  the specified Game URL. For this option to work, 
--  you must first create a game for your bot via 
--  @Botfather and accept the terms. Otherwise, you 
--  may use links like t.me/your_bot?start=XXXX that 
--  open your bot with a parameter.
answerCallbackQuery :: AnswerCallbackQueryRequest ->  ClientM (Response Bool)
answerCallbackQuery = client (Proxy @AnswerCallbackQuery)
