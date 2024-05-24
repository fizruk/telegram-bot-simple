{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.InputPollOption where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.MessageEntity
import Telegram.Bot.API.Types.ParseMode

-- ** 'InputPollOption'

-- | This object contains information about one answer option in a poll to send.
data InputPollOption = InputPollOption
  { inputPollOptionText :: Text -- ^ Option text, 1-100 characters.
  , inputPollOptionTextParseMode :: Maybe ParseMode -- ^ Mode for parsing entities in the text. See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details. Currently, only custom emoji entities are allowed.
  , inputPollOptionTextEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the poll option text. It can be specified instead of @text_parse_mode@.
  }
  deriving (Generic, Show)

instance ToJSON   InputPollOption where toJSON = gtoJSON
instance FromJSON InputPollOption where parseJSON = gparseJSON
