{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ExternalReplyInfo where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.Animation
import Telegram.Bot.API.Types.Audio
import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.Contact
import Telegram.Bot.API.Types.Dice
import Telegram.Bot.API.Types.Game
import Telegram.Bot.API.Types.Document
import Telegram.Bot.API.Types.Invoice
import Telegram.Bot.API.Types.Location
import Telegram.Bot.API.Types.MessageOrigin
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Types.Poll
import Telegram.Bot.API.Types.Sticker
import Telegram.Bot.API.Types.Story
import Telegram.Bot.API.Types.Venue
import Telegram.Bot.API.Types.Video
import Telegram.Bot.API.Types.VideoNote
import Telegram.Bot.API.Types.Voice


-- ** 'ExternalReplyInfo'

-- | This object contains information about a message that is being replied to, which may come from another chat or forum topic.
data ExternalReplyInfo = ExternalReplyInfo
      { externalReplyInfoOrigin :: MessageOrigin -- ^ Origin of the message replied to by the given message.
      , externalReplyInfoChat :: Maybe Chat -- ^ Chat the original message belongs to. Available only if the chat is a supergroup or a channel.
      , externalReplyInfoMessageId :: Maybe MessageId -- ^ Unique message identifier inside the original chat. Available only if the original chat is a supergroup or a channel.

      -- , externalReplyInfoLinkPreviewsOptions
      , externalReplyInfoAnimation :: Maybe Animation -- ^ Message is an animation, information about the animation.
      , externalReplyInfoAudio :: Maybe Audio -- ^ Message is an audio file, information about the file.
      , externalReplyInfoDocument :: Maybe Document -- ^ Message is a general file, information about the file.
      , externalReplyInfoPhoto :: Maybe [PhotoSize] -- ^ Message is a photo, available sizes of the photo.
      , externalReplyInfoSticker :: Maybe Sticker -- ^ Message is a sticker, information about the sticker.
      , externalReplyInfoStory :: Maybe Story -- ^ Message is a forwarded story.
      , externalReplyInfoVideo :: Maybe Video -- ^ Message is a video, information about the video.
      , externalReplyInfoVideoNote :: Maybe VideoNote -- ^ Message is a video note, information about the video message
      , externalReplyInfoVoice :: Maybe Voice -- ^ Message is a voice message, information about the file
      , externalReplyInfoHasMediaSpoiler :: Maybe Bool -- ^ 'True', if the message media is covered by a spoiler animation.
      , externalReplyInfoContact :: Maybe Contact -- ^ Message is a shared contact, information about the contact.
      , externalReplyInfoDice :: Maybe Dice -- ^ Message is a dice with random value.
      , externalReplyInfoGame :: Maybe Game -- ^ Message is a game, information about the game.

      -- , externalReplyInfoGiveaway
      -- , externalReplyInfoGiveawayWinners
      , externalReplyInfoInvoice :: Maybe Invoice -- ^ Message is an invoice for a payment, information about the invoice.
      , externalReplyInfoLocation :: Maybe Location -- ^ Message is a shared location, information about the location.
      , externalReplyInfoPoll :: Maybe Poll -- ^ Message is a native poll, information about the poll.
      , externalReplyInfoVenue :: Maybe Venue -- ^ Message is a venue, information about the venue.
      }
  deriving (Generic, Show)

instance ToJSON   ExternalReplyInfo where toJSON = gtoJSON
instance FromJSON ExternalReplyInfo where parseJSON = gparseJSON
