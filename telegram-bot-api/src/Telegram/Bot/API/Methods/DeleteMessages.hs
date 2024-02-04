{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.DeleteMessages where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.TH
import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'deleteMessages'


-- | Use this method to delete multiple messages simultaneously.
-- If some of the specified messages can't be found, they are skipped.
-- Returns 'True' on success.
type DeleteMessages = "deleteMessages"
  :> ReqBody '[JSON] DeleteMessagesRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to delete message in chat.
-- On success, the sent Bool is returned.
deleteMessages :: DeleteMessagesRequest -> ClientM (Response Bool)
deleteMessages = client (Proxy @DeleteMessages)

-- | Request parameters for 'deleteMessages'.
data DeleteMessagesRequest = DeleteMessagesRequest
  { deleteMessagesRequestChatId :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format \@channelusername).
  , deleteMessagesRequestMessageIds :: [MessageId] -- ^ Identifiers of 1-100 messages to delete. See @deleteMessage@ for limitations on which messages can be deleted.
  } deriving Generic

instance ToJSON DeleteMessagesRequest where toJSON = gtoJSON
instance FromJSON DeleteMessagesRequest where parseJSON = gparseJSON

makeDefault ''DeleteMessagesRequest
