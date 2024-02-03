{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.TextQuote where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.MessageEntity
import Telegram.Bot.API.Internal.Utils

-- ** 'TextQuote'

-- | This object contains information about the quoted part of a message that is replied to by the given message.
data TextQuote = TextQuote
  { textQuoteText :: Text -- ^ Text of the quoted part of a message that is replied to by the given message.
  , textQuoteEntities :: Maybe [MessageEntity] -- ^ Special entities that appear in the quote. Currently, only @bold@, @italic@, @underline@, @strikethrough@, @spoiler@, and @custom_emoji@ entities are kept in quotes.
  , textQuotePosition :: Int -- ^ Approximate quote position in the original message in UTF-16 code units as specified by the sender.
  , textQuoteIsManual :: Maybe Bool -- ^ 'True', if the quote was chosen manually by the message sender. Otherwise, the quote was added automatically by the server.
  } deriving (Show, Generic)

instance ToJSON   TextQuote where toJSON = gtoJSON
instance FromJSON TextQuote where parseJSON = gparseJSON
