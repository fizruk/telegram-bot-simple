{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.UnbanChatMember where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- | Request parameters for 'unbanChatMember'.
data UnbanChatMemberRequest = UnbanChatMemberRequest
  { unbanChatMemberChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , unbanChatMemberUserId :: UserId -- ^ Unique identifier of the target user
  , unbanChatMemberOnlyIfBanned :: Maybe Bool -- ^ Do nothing if the user is not banned
  }
  deriving Generic

instance ToJSON   UnbanChatMemberRequest where toJSON = gtoJSON
instance FromJSON UnbanChatMemberRequest where parseJSON = gparseJSON

type UnbanChatMember = "unbanChatMember"
  :> ReqBody '[JSON] UnbanChatMemberRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to unban a previously
--   banned user in a supergroup or channel.
--   The user will not return to the group
--   or channel automatically, but will be
--   able to join via link, etc. The bot must
--   be an administrator for this to work. By
--   default, this method guarantees that after
--   the call the user is not a member of the chat,
--   but will be able to join it. So if the user is
--   a member of the chat they will also be removed
--   from the chat. If you don't want this, use the
--   parameter only_if_banned.
--   Returns True on success.
unbanChatMember :: UnbanChatMemberRequest ->  ClientM (Response Bool)
unbanChatMember = client (Proxy @UnbanChatMember)

makeDefault ''UnbanChatMemberRequest
