{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Telegram.Bot.API.Types.Sticker where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common 
import Telegram.Bot.API.Types.File
import Telegram.Bot.API.Types.MaskPosition
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Internal.Utils

-- ** 'Sticker'

-- | This object represents a sticker.
data Sticker = Sticker
  { stickerFileId       :: FileId             -- ^ Identifier for this file, which can be used to download or reuse the file.
  , stickerFileUniqueId :: FileId             -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , stickerWidth        :: Int              -- ^ Sticker width.
  , stickerHeight       :: Int              -- ^ Sticker height.
  , stickerIsAnimated   :: Bool               -- ^ 'True', if the sticker is animated.
  , stickerIsVideo      :: Bool               -- ^ 'True', if the sticker is a video sticker.
  , stickerThumb        :: Maybe PhotoSize    -- ^ Sticker thumbnail in the .WEBP or .JPG format.
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
  , stickerSetThumb         :: Maybe PhotoSize -- ^ Sticker set thumbnail in the .WEBP or .TGS format.
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
