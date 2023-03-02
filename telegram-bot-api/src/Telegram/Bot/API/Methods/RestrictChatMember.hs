{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.RestrictChatMember where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'restrictChatMember'

-- | Request parameters for 'restrictChatMember'.
data RestrictChatMemberRequest = RestrictChatMemberRequest
  { restrictChatMemberChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , restrictChatMemberUserId :: UserId -- ^ Unique identifier of the target user.
  , restrictChatMemberPermissions :: ChatPermissions -- ^ A JSON-serialized object for new user permissions.
  , restrictChatMemberUseIndependentChatPermissions :: Maybe Bool -- ^ Pass 'True' if chat permissions are set independently. Otherwise, the @can_send_other_messages@ and @can_add_web_page_previews@ permissions will imply the @can_send_messages@, @can_send_audios@, @can_send_documents@, @can_send_photos@, @can_send_videos@, @can_send_video_notes@, and @can_send_voice_notes@ permissions; the @can_send_polls@ permission will imply the @can_send_messages@ permission.
  , restrictChatMemberUntilDate :: Maybe Int -- ^ Date when restrictions will be lifted for the user, unix time. If user is restricted for more than 366 days or less than 30 seconds from the current time, they are considered to be restricted forever.
  }
  deriving Generic

instance ToJSON   RestrictChatMemberRequest where toJSON = gtoJSON
instance FromJSON RestrictChatMemberRequest where parseJSON = gparseJSON

type RestrictChatMember = "restrictChatMember"
  :> ReqBody '[JSON] RestrictChatMemberRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to restrict a user
--   in a supergroup. The bot must be an
--   administrator in the supergroup for
--   this to work and must have the appropriate
--   administrator rights. Pass True for all
--   permissions to lift restrictions from a
--   user.
--   Returns True on success.
restrictChatMember :: RestrictChatMemberRequest ->  ClientM (Response Bool)
restrictChatMember = client (Proxy @RestrictChatMember)

makeDefault ''RestrictChatMemberRequest
