{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.BanChatMember where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'banChatMember'

-- | Request parameters for 'banChatMember'.
data BanChatMemberRequest = BanChatMemberRequest
  { banChatMemberChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , banChatMemberUserId :: UserId -- ^ Unique identifier of the target user
  , banChatMemberUntilDate :: Maybe Int -- ^ Date when the user will be unbanned, unix time. If user is banned for more than 366 days or less than 30 seconds from the current time they are considered to be banned forever. Applied for supergroups and channels only.
  , banChatMemberRevokeMessages :: Maybe Bool -- ^ Pass True to delete all messages from the chat for the user that is being removed. If False, the user will be able to see messages in the group that were sent before the user was removed. Always True for supergroups and channels.
  }
  deriving Generic

instance ToJSON   BanChatMemberRequest where toJSON = gtoJSON
instance FromJSON BanChatMemberRequest where parseJSON = gparseJSON

type BanChatMember = "banChatMember"
  :> ReqBody '[JSON] BanChatMemberRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to ban a user in a
--   group, a supergroup or a channel.
--   In the case of supergroups and channels,
--   the user will not be able to return to
--   the chat on their own using invite links,
--   etc., unless unbanned first. The bot must
--   be an administrator in the chat for this
--   to work and must have the appropriate
--   administrator rights.
--   Returns True on success.
banChatMember :: BanChatMemberRequest ->  ClientM (Response Bool)
banChatMember = client (Proxy @BanChatMember)

makeDefault ''BanChatMemberRequest
