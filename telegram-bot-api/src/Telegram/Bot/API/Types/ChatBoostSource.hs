{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.API.Types.ChatBoostSource where

import Data.Aeson (FromJSON (..), ToJSON (..), KeyValue ((.=)), Value (..), withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as Text

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.User

-- ** 'ChatBoostSource'

-- | This object describes the source of a chat boost. It can be one of
--
-- * ChatBoostSourcePremium
-- * ChatBoostSourceGiftCode
-- * ChatBoostSourceGiveaway
--
data ChatBoostSource
  -- | The boost was obtained by subscribing to Telegram Premium or by gifting a Telegram Premium subscription to another user.
  = ChatBoostSourcePremium
      { chatBoostSourcePremiumSource :: Text -- ^ Source of the boost, always “premium”.
      , chatBoostSourcePremiumUser :: User -- ^ User that boosted the chat.
      }
  -- | The boost was obtained by the creation of Telegram Premium gift codes to boost a chat. Each such code boosts the chat 4 times for the duration of the corresponding Telegram Premium subscription.
  | ChatBoostSourceGiftCode
      { chatBoostSourceGiftCodeSource :: Text -- ^ Source of the boost, always “gift_code”.
      , chatBoostSourceGiftCodeUser :: User -- ^ User for which the gift code was created.
      }
  -- | The boost was obtained by the creation of a Telegram Premium giveaway. This boosts the chat 4 times for the duration of the corresponding Telegram Premium subscription.
  | ChatBoostSourceGiveaway
      { chatBoostSourceGiveawaySource :: Text -- ^ Source of the boost, always “giveaway”.
      , chatBoostSourceGiveawayGiveawayMessageId :: MessageId -- ^ Identifier of a message in the chat with the giveaway; the message could have been deleted already. May be 0 if the message isn't sent yet.
      , chatBoostSourceGiveawayUser :: Maybe User -- ^ User that won the prize in the giveaway if any
      , chatBoostSourceGiveawayIsUnclaimed :: Maybe Bool -- ^ 'True', if the giveaway was completed, but there was no user to win the prize
      }
  deriving (Generic, Show)

instance ToJSON ChatBoostSource where
  toJSON = \case
    ChatBoostSourcePremium _s u -> addJsonFields (Object mempty)
      ["source" .= ("premium" :: Text), "user" .= u]
    ChatBoostSourceGiftCode _s u -> addJsonFields (Object mempty)
      ["source" .= ("gift_code" :: Text), "user" .= u]
    ChatBoostSourceGiveaway _s gm u iu -> addJsonFields (Object mempty)
      [ "source" .= ("giveaway" :: Text)
      , "giveaway_message_id" .= gm
      , "user" .= u
      , "is_unclaimed" .= iu
      ]
      

instance FromJSON ChatBoostSource where
  parseJSON = withObject "ChatBoostSource" \o ->
    (o .: "source" :: Parser Text) >>= \case
    "premium" -> ChatBoostSourcePremium
      <$> o .: "source"
      <*> o .: "user"
    "gift_code" -> ChatBoostSourceGiftCode
      <$> o .: "source"
      <*> o .: "user"
    "giveaway" -> ChatBoostSourceGiveaway
      <$> o .: "source"
      <*> o .: "giveaway_message_id"
      <*> o .:? "user"
      <*> o .:? "is_unclaimed"
    t -> fail $ Text.unpack ("Unknown source: " <> t)
