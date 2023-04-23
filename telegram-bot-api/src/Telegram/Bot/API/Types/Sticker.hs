{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Telegram.Bot.API.Types.Sticker where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common 
import Telegram.Bot.API.Types.File
import Telegram.Bot.API.Types.InputMedia
import Telegram.Bot.API.Types.MaskPosition
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Internal.Utils

-- ** 'InputSticker'

data InputSticker = InputSticker
  { inputStickerSticker :: InputFile -- ^ The added sticker. Pass a file_id as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, upload a new one using @multipart/form-data@, or pass @attach://<file_attach_name>@ to upload a new one using @multipart/form-data@ under @<file_attach_name>@ name. Animated and video stickers can't be uploaded via HTTP URL.
  , inputStickerEmojiList :: [Text] -- ^ List of 1-20 emoji associated with the sticker.
  , inputStickerMaskPosition :: Maybe MaskPosition -- ^ Position where the mask should be placed on faces. For “mask” stickers only.
  , inputStickerKeywords :: Maybe [Text] -- ^ List of 0-20 search keywords for the sticker with total length of up to 64 characters. For “regular” and “custom_emoji” stickers only.
  }
  deriving (Generic, Show)

instance ToJSON   InputSticker where toJSON = gtoJSON

-- ** 'Sticker'

-- | This object represents a sticker.
data Sticker = Sticker
  { stickerFileId       :: FileId             -- ^ Identifier for this file, which can be used to download or reuse the file.
  , stickerFileUniqueId :: FileId             -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , stickerWidth        :: Int              -- ^ Sticker width.
  , stickerHeight       :: Int              -- ^ Sticker height.
  , stickerIsAnimated   :: Bool               -- ^ 'True', if the sticker is animated.
  , stickerIsVideo      :: Bool               -- ^ 'True', if the sticker is a video sticker.
  , stickerThumbnail    :: Maybe PhotoSize    -- ^ Sticker thumbnail in the .WEBP or .JPG format.
  , stickerEmoji        :: Maybe Text         -- ^ Emoji associated with the sticker.
  , stickerSetName      :: Maybe Text         -- ^ Name of the sticker set to which the sticker belongs.
  , stickerPremiumAnimation :: Maybe File    -- ^ For premium regular stickers, premium animation for the sticker.
  , stickerMaskPosition :: Maybe MaskPosition -- ^ For mask stickers, the position where the mask should be placed.
  , stickerCustomEmojiId :: Maybe Text        -- ^ For custom emoji stickers, unique identifier of the custom emoji.
  , stickerFileSize     :: Maybe Integer      -- ^ File size in bytes.
  , stickerNeedsRepainting  :: Maybe Bool      -- ^ Pass `True` if stickers in the sticker set must be repainted to the color of text when used in messages, the accent color if used as emoji status, white on chat photos, or another appropriate color based on context; for custom emoji sticker sets only.
  }
  deriving (Generic, Show)

instance ToJSON   Sticker where toJSON = gtoJSON
instance FromJSON Sticker where parseJSON = gparseJSON

-- ** 'StickerSet'

-- | This object represents a sticker set.
data StickerSet = StickerSet
  { stickerSetName          :: Text            -- ^ Sticker set name.
  , stickerSetTitle         :: Text            -- ^ Sticker set title.
  , stickerSetType          :: StickerSetType  -- ^ Type of stickers in the set, currently one of “regular”, “mask”, “custom_emoji”.
  , stickerSetIsAnimated    :: Bool            -- ^ 'True', if the sticker set contains animated stickers.
  , stickerSetIsVideo       :: Bool            -- ^ 'True', if the sticker is a video sticker.
  , stickerSetContainsMasks :: Maybe Bool      -- ^ True, if the sticker set contains masks.
  , stickerSetStickers      :: [Sticker]       -- ^ List of all set stickers.
  , stickerSetThumbnail     :: Maybe PhotoSize -- ^ Sticker set thumbnail in the .WEBP or .TGS format.
  }
  deriving (Generic, Show)

instance ToJSON   StickerSet where toJSON = gtoJSON
instance FromJSON StickerSet where parseJSON = gparseJSON


-- | Type of stickers in the set, currently one of “regular”, “mask”, “custom_emoji”.
data StickerSetType
  = StickerSetTypeRegular
  | StickerSetTypeMask
  | StickerSetTypeCustomEmoji
  deriving (Eq, Show, Generic)

instance ToJSON   StickerSetType where toJSON = gtoJSON
instance FromJSON StickerSetType where parseJSON = gparseJSON
