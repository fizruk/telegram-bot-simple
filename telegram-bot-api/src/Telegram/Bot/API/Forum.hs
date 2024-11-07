{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE LambdaCase #-}

module Telegram.Bot.API.Forum where

import Data.Aeson (ToJSON (..))
import Data.Proxy
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests (Response)
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'getForumTopicIconStickers'

type GetForumTopicIconStickers
  = "getForumTopicIconStickers"
  :> Post '[JSON] (Response [Sticker])

-- | Use this method to get custom emoji stickers, which can be used as a forum topic icon by any user.
-- Requires no parameters. Returns an '[Sticker]' objects.
getForumTopicIconStickers :: ClientM (Response [Sticker])
getForumTopicIconStickers = client (Proxy @GetForumTopicIconStickers)

-- ** 'createForumTopic'

data CreateForumTopicRequest = CreateForumTopicRequest
  { createForumTopicRequestChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername).
  , createForumTopicName :: Text -- ^ Topic name, 1-128 characters.
  , createForumTopicIconColor :: Maybe Integer -- ^ Color of the topic icon in RGB format. Currently, must be one of @7322096 (0x6FB9F0), 16766590 (0xFFD67E), 13338331 (0xCB86DB), 9367192 (0x8EEE98), 16749490 (0xFF93B2), or 16478047 (0xFB6F5F)@.
  , createForumTopicIconCustomEmojiId :: Maybe Text -- ^ Unique identifier of the custom emoji shown as the topic icon. Use 'getForumTopicIconStickers' to get all allowed custom emoji identifiers.
  }
  deriving Generic

instance ToJSON CreateForumTopicRequest where toJSON = gtoJSON

type CreateForumTopic
  = "createForumTopic"
  :> ReqBody '[JSON] CreateForumTopicRequest
  :> Post '[JSON] (Response ForumTopic)

-- | Use this method to create a topic in a forum supergroup chat. The bot must be an administrator in the chat for this to work and must have the @can_manage_topics@ administrator rights. Returns information about the created topic as a 'ForumTopic' object.
createForumTopic :: CreateForumTopicRequest -> ClientM (Response ForumTopic)
createForumTopic = client (Proxy @CreateForumTopic)


-- ** 'editForumTopic'

data EditForumTopicRequest = EditForumTopicRequest
  { editForumTopicChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername).
  , editForumTopicMessageThreadId :: MessageThreadId -- ^ Unique identifier for the target message thread of the forum topic.
  , editForumTopicName :: Maybe Text -- ^ New topic name, 0-128 characters. If not specified or empty, the current name of the topic will be kept.
  , editForumTopicIconCustomEmojiId :: Maybe Text -- ^ New unique identifier of the custom emoji shown as the topic icon. Use 'getForumTopicIconStickers' to get all allowed custom emoji identifiers. Pass an empty string to remove the icon. If not specified, the current icon will be kept.
  }
  deriving Generic

instance ToJSON EditForumTopicRequest where toJSON = gtoJSON

type EditForumTopic
  = "editForumTopic"
  :> ReqBody '[JSON] EditForumTopicRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to edit name and icon of a topic in a forum supergroup chat. The bot must be an administrator in the chat for this to work and must have @can_manage_topics@ administrator rights, unless it is the creator of the topic. Returns 'True' on success.
editForumTopic :: EditForumTopicRequest -> ClientM (Response Bool)
editForumTopic = client (Proxy @EditForumTopic)

-- ** 'closeForumTopic'

data CloseForumTopicRequest = CloseForumTopicRequest
  { closeForumTopicChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername).
  , closeForumTopicMessageThreadId :: MessageThreadId -- ^ Unique identifier for the target message thread of the forum topic.
  }
  deriving Generic

instance ToJSON CloseForumTopicRequest where toJSON = gtoJSON

type CloseForumTopic
  = "closeForumTopic"
  :> ReqBody '[JSON] CloseForumTopicRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to close an open topic in a forum supergroup chat. The bot must be an administrator in the chat for this to work and must have the @can_manage_topics@ administrator rights, unless it is the creator of the topic. Returns 'True' on success.
closeForumTopic :: CloseForumTopicRequest -> ClientM (Response Bool)
closeForumTopic = client (Proxy @CloseForumTopic)

-- ** 'reopenForumTopic'

data ReopenForumTopicRequest = ReopenForumTopicRequest
  { reopenForumTopicChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername).
  , reopenForumTopicMessageThreadId :: MessageThreadId -- ^ Unique identifier for the target message thread of the forum topic.
  }
  deriving Generic

instance ToJSON ReopenForumTopicRequest where toJSON = gtoJSON

type ReopenForumTopic
  = "reopenForumTopic"
  :> ReqBody '[JSON] ReopenForumTopicRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to reopen a closed topic in a forum supergroup chat. The bot must be an administrator in the chat for this to work and must have the @can_manage_topics@ administrator rights, unless it is the creator of the topic. Returns 'True' on success.
reopenForumTopic :: ReopenForumTopicRequest -> ClientM (Response Bool)
reopenForumTopic = client (Proxy @ReopenForumTopic)

-- ** 'deleteForumTopic'

data DeleteForumTopicRequest = DeleteForumTopicRequest
  { deleteForumTopicChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername).
  , deleteForumTopicMessageThreadId :: MessageThreadId -- ^ Unique identifier for the target message thread of the forum topic.
  }
  deriving Generic

instance ToJSON DeleteForumTopicRequest where toJSON = gtoJSON

type DeleteForumTopic
  = "deleteForumTopic"
  :> ReqBody '[JSON] DeleteForumTopicRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to delete a forum topic along with all its messages in a forum supergroup chat. The bot must be an administrator in the chat for this to work and must have the @can_delete_messages@ administrator rights. Returns 'True' on success.
deleteForumTopic :: DeleteForumTopicRequest -> ClientM (Response Bool)
deleteForumTopic = client (Proxy @DeleteForumTopic)

-- ** 'unpinAllForumTopicMessages'

data UnpinAllForumTopicMessagesRequest = UnpinAllForumTopicMessagesRequest
  { unpinAllForumTopicMessagesChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  , unpinAllForumTopicMessagesMessageThreadId :: MessageThreadId -- ^ Unique identifier for the target message thread of the forum topic.
  }
  deriving Generic

instance ToJSON UnpinAllForumTopicMessagesRequest where toJSON = gtoJSON

type UnpinAllForumTopicMessages
  = "unpinAllForumTopicMessages"
  :> ReqBody '[JSON] UnpinAllForumTopicMessagesRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to clear the list of pinned messages in a forum topic. The bot must be an administrator in the chat for this to work and must have the @can_pin_messages@ administrator right in the supergroup. Returns 'True' on success.
unpinAllForumTopicMessages :: UnpinAllForumTopicMessagesRequest -> ClientM (Response Bool)
unpinAllForumTopicMessages = client (Proxy @UnpinAllForumTopicMessages)

-- ** 'unpinAllGeneralForumTopicMessages'

type UnpinAllGeneralForumTopicMessages = "unpinAllGeneralForumTopicMessages"
  :> RequiredQueryParam "chat_id" SomeChatId -- ^ Unique identifier for the target chat or username of the target @supergroup@ (in the format \@supergroupusername)
  :> Post '[JSON] (Response Bool)

-- | Use this method to clear the list of pinned messages
--   in a @General@ forum topic. The bot must be an administrator
--   in the chat for this to work and must have the 'can_pin_messages' administrator right
--   in the supergroup. Returns 'True' on success.
unpinAllGeneralForumTopicMessages :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Bool)
unpinAllGeneralForumTopicMessages = client (Proxy @UnpinAllGeneralForumTopicMessages)


-- ** 'editGeneralForumTopic'

data EditGeneralForumTopicRequest = EditGeneralForumTopicRequest
  { editGeneralForumTopicChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername).
  , editGeneralForumTopicName :: Text -- ^ New topic name, 1-128 characters.
  }
  deriving Generic

instance ToJSON EditGeneralForumTopicRequest where toJSON = gtoJSON

type EditGeneralForumTopic
  = "editGeneralForumTopic"
  :> ReqBody '[JSON] EditGeneralForumTopicRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to edit the name of the @General@ topic in a forum supergroup chat. The bot must be an administrator in the chat for this to work and must have @can_manage_topics@ administrator rights. Returns 'True' on success.
editGeneralForumTopic :: EditGeneralForumTopicRequest -> ClientM (Response Bool)
editGeneralForumTopic = client (Proxy @EditGeneralForumTopic)

-- ** 'closeGeneralForumTopic'

newtype CloseGeneralForumTopicRequest = CloseGeneralForumTopicRequest
  { closeGeneralForumTopicChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername).
  }
  deriving Generic

instance ToJSON CloseGeneralForumTopicRequest where toJSON = gtoJSON

type CloseGeneralForumTopic
  = "closeGeneralForumTopic"
  :> ReqBody '[JSON] CloseGeneralForumTopicRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to close an open @General@ topic in a forum supergroup chat. The bot must be an administrator in the chat for this to work and must have the @can_manage_topics@ administrator rights. Returns 'True' on success.
closeGeneralForumTopic :: CloseGeneralForumTopicRequest -> ClientM (Response Bool)
closeGeneralForumTopic = client (Proxy @CloseGeneralForumTopic)

-- ** 'reopenGeneralForumTopic'

newtype ReopenGeneralForumTopicRequest = ReopenGeneralForumTopicRequest
  { reopenGeneralForumTopicRequestChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername).
  }
  deriving Generic

instance ToJSON ReopenGeneralForumTopicRequest where toJSON = gtoJSON

type ReopenGeneralForumTopic
  = "reopenGeneralForumTopic"
  :> ReqBody '[JSON] ReopenGeneralForumTopicRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to reopen a closed @General@ topic in a forum supergroup chat. The bot must be an administrator in the chat for this to work and must have the @can_manage_topics@ administrator rights. The topic will be automatically unhidden if it was hidden. Returns 'True' on success.
reopenGeneralForumTopic :: ReopenGeneralForumTopicRequest -> ClientM (Response Bool)
reopenGeneralForumTopic = client (Proxy @ReopenGeneralForumTopic)

-- ** 'hideGeneralForumTopic'

newtype HideGeneralForumTopicRequest = HideGeneralForumTopicRequest
  { hideGeneralForumTopicChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername).
  }
  deriving Generic

instance ToJSON HideGeneralForumTopicRequest where toJSON = gtoJSON

type HideGeneralForumTopic
  = "hideGeneralForumTopic"
  :> ReqBody '[JSON] HideGeneralForumTopicRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to hide the @General@ topic in a forum supergroup chat. The bot must be an administrator in the chat for this to work and must have the @can_manage_topics@ administrator rights. The topic will be automatically closed if it was open. Returns 'True' on success.
hideGeneralForumTopic :: HideGeneralForumTopicRequest -> ClientM (Response Bool)
hideGeneralForumTopic = client (Proxy @HideGeneralForumTopic)

-- ** 'unhideGeneralForumTopic'

newtype UnhideGeneralForumTopicRequest = UnhideGeneralForumTopicRequest
  { unhideGeneralForumTopicChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  }
  deriving Generic

instance ToJSON UnhideGeneralForumTopicRequest where toJSON = gtoJSON

type UnhideGeneralForumTopic
  = "unhideGeneralForumTopic"
  :> ReqBody '[JSON] UnhideGeneralForumTopicRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to unhide the @General@ topic in a forum supergroup chat. The bot must be an administrator in the chat for this to work and must have the @can_manage_topics@ administrator rights. Returns 'True' on success.
unhideGeneralForumTopic :: UnhideGeneralForumTopicRequest -> ClientM (Response Bool)
unhideGeneralForumTopic = client (Proxy @UnhideGeneralForumTopic)

foldMap makeDefault
  [ ''UnhideGeneralForumTopicRequest
  , ''HideGeneralForumTopicRequest
  , ''ReopenGeneralForumTopicRequest
  , ''CloseGeneralForumTopicRequest
  , ''EditGeneralForumTopicRequest
  , ''UnpinAllForumTopicMessagesRequest
  , ''DeleteForumTopicRequest
  , ''ReopenForumTopicRequest
  , ''CloseForumTopicRequest
  , ''EditForumTopicRequest
  , ''CreateForumTopicRequest
  ]
