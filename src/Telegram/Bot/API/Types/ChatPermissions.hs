{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.ChatPermissions where

import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'ChatPermissions'

-- | Describes actions that a non-administrator user is allowed to take in a chat.
data ChatPermissions = ChatPermissions
  { chatPermissionsCanSendMessages :: Maybe Bool       -- ^ 'True', if the user is allowed to send text messages, contacts, locations and venues.
  , chatPermissionsCanSendAudios     :: Maybe Bool     -- ^ 'True', if the user is allowed to send audios.
  , chatPermissionsCanSendDocuments  :: Maybe Bool     -- ^ 'True', if the user is allowed to send documents.
  , chatPermissionsCanSendPhotos     :: Maybe Bool     -- ^ 'True', if the user is allowed to send photos.
  , chatPermissionsCanSendVideos     :: Maybe Bool     -- ^ 'True', if the user is allowed to send videos.
  , chatPermissionsCanSendVideoNotes :: Maybe Bool     -- ^ 'True', if the user is allowed to send video notes.
  , chatPermissionsCanSendVoiceNotes :: Maybe Bool     -- ^ 'True', if the user is allowed to send voice notes.
  , chatPermissionsCanSendPolls :: Maybe Bool          -- ^ 'True', if the user is allowed to send polls, implies can_send_messages.
  , chatPermissionsCanSendOtherMessages :: Maybe Bool  -- ^ 'True', if the user is allowed to send animations, games, stickers and use inline bots, implies can_send_media_messages.
  , chatPermissionsCanAddWebPagePreviews :: Maybe Bool -- ^ 'True', if the user is allowed to add web page previews to their messages, implies can_send_media_messages.
  , chatPermissionsCanChangeInfo :: Maybe Bool         -- ^ 'True', if the user is allowed to change the chat title, photo and other settings. Ignored in public supergroups
  , chatPermissionsCanInviteUsers :: Maybe Bool        -- ^ 'True', if the user is allowed to invite new users to the chat.
  , chatPermissionsCanPinMessages :: Maybe Bool        -- ^ 'True', if the user is allowed to pin messages. Ignored in public supergroups.
  , chatPermissionsCanManageTopics :: Maybe Bool       -- ^ 'True', if the user is allowed to create forum topics. If omitted defaults to the value of can_pin_messages.
  }
  deriving (Generic, Show)

deriveJSON' ''ChatPermissions
