{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetStickerEmojiList where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Internal.TH

-- ** 'setStickerEmojiList'

-- | Request parameters for 'setStickerEmojiList'.
data SetStickerEmojiListRequest = SetStickerEmojiListRequest
  { setStickerEmojiListSticker :: Text -- ^ File identifier of the sticker
  , setStickerEmojiListEmojiList :: [Text] -- ^ A JSON-serialized list of 1-20 emoji associated with the sticker
  }
  deriving Generic

instance ToJSON   SetStickerEmojiListRequest where toJSON = gtoJSON
instance FromJSON SetStickerEmojiListRequest where parseJSON = gparseJSON

type SetStickerEmojiList = "setStickerEmojiList"
  :> ReqBody '[JSON] SetStickerEmojiListRequest
  :> Post '[JSON] (Response Bool)
 
-- | Use this method to change the list of
--   emoji assigned to a regular or custom emoji sticker.
--   The sticker must belong to a sticker set created by the bot.
--   Returns 'True' on success.
setStickerEmojiList :: SetStickerEmojiListRequest ->  ClientM (Response Bool)
setStickerEmojiList = client (Proxy @SetStickerEmojiList)

makeDefault ''SetStickerEmojiListRequest
