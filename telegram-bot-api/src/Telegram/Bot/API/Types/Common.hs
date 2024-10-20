{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Telegram.Bot.API.Types.Common where

import Data.Aeson (FromJSON (..), ToJSON (..), KeyValue ((.=)))
import Data.Aeson.Types (Pair)
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData (..))

import Telegram.Bot.API.Internal.Utils

-- | Unique identifier for this file.
newtype FileId = FileId Text
  deriving (Eq, Show, ToJSON, FromJSON)

instance ToHttpApiData FileId where
  toUrlPiece = coerce

newtype Seconds = Seconds Int
  deriving (Eq, Show, Num, ToJSON, FromJSON)

-- | Unique identifier for this user or bot.
newtype UserId = UserId Integer
  deriving (Eq, Show, ToJSON, FromJSON)

instance ToHttpApiData UserId where
  toUrlPiece = pack . show @Integer . coerce

-- | Unique identifier of the boost.
newtype BoostId = BoostId Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Unique identifier for this chat.
newtype ChatId = ChatId Integer
  deriving (Eq, Show, ToJSON, FromJSON, Hashable)

instance ToHttpApiData ChatId where
  toUrlPiece a = pack . show @Integer $ coerce a

-- | Unique message identifier inside this chat.
newtype MessageId = MessageId Integer
  deriving (Eq, Show, ToJSON, FromJSON, Hashable)

instance ToHttpApiData MessageId where
  toUrlPiece a = pack . show @Integer $ coerce a

-- | Unique identifier of a message thread to which the message belongs; for supergroups only.
newtype MessageThreadId = MessageThreadId Integer
  deriving (Eq, Show, ToJSON, FromJSON, Hashable)

instance ToHttpApiData MessageThreadId where
  toUrlPiece a = pack . show @Integer $ coerce a

newtype InlineMessageId = InlineMessageId Text
  deriving (Eq, Show, ToJSON, FromJSON, Hashable)

-- | The unique identifier of a media message group a message belongs to.
newtype MediaGroupId = MediaGroupId Text
  deriving (Eq, Show, ToJSON, FromJSON)

-- | Signed 32-bit identifier of the request, which will be received back in the 'UserShared' or 'ChatShared' object. Must be unique within the message.
newtype RequestId = RequestId Integer
  deriving (Eq, Show, ToJSON, FromJSON)

-- | Unique poll identifier.
newtype PollId = PollId Text
  deriving (Eq, Show, ToJSON, FromJSON)

newtype ShippingOptionId = ShippingOptionId Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype WebAppInfo = WebAppInfo { webAppInfoUrl :: Text }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype CallbackQueryId = CallbackQueryId Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Unique identifier of the business connection.
newtype BusinessConnectionId = BusinessConnectionId Text
  deriving (Eq, Show, Generic, ToHttpApiData, ToJSON, FromJSON)

-- | Unique identifier for the target chat
-- or username of the target channel (in the format @\@channelusername@).
data SomeChatId
  = SomeChatId ChatId       -- ^ Unique chat ID.
  | SomeChatUsername Text   -- ^ Username of the target channel.
  deriving (Generic, Show)

instance ToJSON   SomeChatId where toJSON = genericSomeToJSON
instance FromJSON SomeChatId where parseJSON = genericSomeParseJSON

instance ToHttpApiData SomeChatId where
  toUrlPiece (SomeChatId chatid) = toUrlPiece chatid
  toUrlPiece (SomeChatUsername name) = name

addType :: Text -> [Pair] -> [Pair]
addType name xs = ("type" .= name) : xs

