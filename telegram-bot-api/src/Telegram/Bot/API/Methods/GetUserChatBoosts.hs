{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetUserChatBoosts where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'getUserChatBoosts'

type GetUserChatBoosts
  = "getUserChatBoosts"
  :> ReqBody '[JSON] GetUserChatBoostsRequest
  :> Post '[JSON] (Response UserChatBoosts)

-- | Use this method to get the list of boosts added to a chat by a user. Requires administrator rights in the chat. Returns a 'UserChatBoosts' object.
getUserChatBoosts :: GetUserChatBoostsRequest -> ClientM (Response UserChatBoosts)
getUserChatBoosts = client (Proxy @GetUserChatBoosts)


-- | Request parameters for 'getUserChatBoosts'.
data GetUserChatBoostsRequest = GetUserChatBoostsRequest
  { getUserChatBoostsRequestChatId :: SomeChatId -- ^ Unique identifier for the chat or username of the channel (in the format @channelusername).
  , getUserChatBoostsRequestUserId :: UserId -- ^ Unique identifier of the target user.
  } deriving (Generic)

instance ToJSON   GetUserChatBoostsRequest where toJSON = gtoJSON
instance FromJSON GetUserChatBoostsRequest where parseJSON = gparseJSON

makeDefault ''GetUserChatBoostsRequest
