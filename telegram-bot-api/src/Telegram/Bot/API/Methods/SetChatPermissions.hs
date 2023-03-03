{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetChatPermissions where


import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'setChatPermissions'

-- | Request parameters for 'setChatPermissions'.
data SetChatPermissionsRequest = SetChatPermissionsRequest
  { setChatPermissionsChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , setChatPermissionsPermissions :: ChatPermissions -- ^ A JSON-serialized object for new default chat permissions.
  , setChatPermissionsUseIndependentChatPermissions :: Maybe Bool -- ^ Pass 'True' if chat permissions are set independently. Otherwise, the @can_send_other_messages@ and @can_add_web_page_previews@ permissions will imply the @can_send_messages@, @can_send_audios@, @can_send_documents@, @can_send_photos@, @can_send_videos@, @can_send_video_notes@, and @can_send_voice_notes@ permissions; the @can_send_polls@ permission will imply the @can_send_messages@ permission.
  }
  deriving Generic

instance ToJSON   SetChatPermissionsRequest where toJSON = gtoJSON
instance FromJSON SetChatPermissionsRequest where parseJSON = gparseJSON

type SetChatPermissions = "setChatPermissions"
  :> ReqBody '[JSON] SetChatPermissionsRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to set default chat
--   permissions for all members. The bot
--   must be an administrator in the group
--   or a supergroup for this to work and must
--   have the can_restrict_members administrator rights.
--   Returns True on success.
setChatPermissions :: SetChatPermissionsRequest ->  ClientM (Response Bool)
setChatPermissions = client (Proxy @SetChatPermissions)

makeDefault ''SetChatPermissionsRequest
