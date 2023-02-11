{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetChatStickerSet where

import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'setChatStickerSet'

type SetChatStickerSet = "setChatStickerSet"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "sticker_set_name" Text
  :> Post '[JSON] (Response Bool)

-- | Use this method to set a new group sticker
--   set for a supergroup. The bot must be an 
--   administrator in the chat for this to work 
--   and must have the appropriate administrator 
--   rights. Use the field can_set_sticker_set 
--   optionally returned in getChat requests to 
--   check if the bot can use this method. 
--   Returns True on success.
setChatStickerSet :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Text -- ^ 	Name of the sticker set to be set as the group sticker set
  -> ClientM (Response Bool)
setChatStickerSet = client (Proxy @SetChatStickerSet)
