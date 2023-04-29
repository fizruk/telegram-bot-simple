{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module Telegram.Bot.API.InlineMode where

import           Data.Aeson                      (FromJSON (..), ToJSON (..))
import           Data.Hashable                   (Hashable)
import           Data.Proxy
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

import           Servant.API
import           Servant.Client                  hiding (Response)

import           Telegram.Bot.API.Internal.Utils
import           Telegram.Bot.API.MakingRequests
import           Telegram.Bot.API.Types
import           Telegram.Bot.API.InlineMode.InlineQueryResult
import Telegram.Bot.API.Internal.TH (makeDefault)

-- * Available types
-- ** User
--
-- | This object represents an incoming inline query. When the user sends an empty query, your bot could return some default or trending results.
--
-- <https://core.telegram.org/bots/api#inline-mode>
data InlineQuery = InlineQuery
  { inlineQueryId       :: InlineQueryId -- ^ Unique query identifier
  , inlineQueryFrom     :: User -- ^ Sender
  , inlineQueryLocation :: Maybe Location -- ^ For bots that require user location, sender location
  , inlineQueryQuery    :: Text -- ^ Text of the query, up to 256 characters
  , inlineQueryOffset   :: Text -- ^ Offset of the results to be returned, can be controlled by bot
  , inlineQueryChatType :: Maybe ChatType -- ^ Type of the chat, from which the inline query was sent. Can be either “sender” for a private chat with the inline query sender, “private”, “group”, “supergroup”, or “channel”. The chat type should be always known for requests sent from official clients and most third-party clients, unless the request was sent from a secret chat.
  } deriving (Generic, Show)

-- | Unique identifier for this query
newtype InlineQueryId = InlineQueryId Text
  deriving (Eq, Show, ToJSON, FromJSON, Hashable, Generic)

-- * Available methods

-- ** answerInlineQuery

type AnswerInlineQuery
  = "answerInlineQuery" :> ReqBody '[JSON] AnswerInlineQueryRequest :> Post '[JSON] (Response Bool)

answerInlineQuery :: AnswerInlineQueryRequest -> ClientM (Response Bool)
answerInlineQuery = client (Proxy @AnswerInlineQuery)

data AnswerInlineQueryRequest = AnswerInlineQueryRequest
  { answerInlineQueryRequestInlineQueryId :: InlineQueryId       -- ^ Unique identifier for the answered query.
  , answerInlineQueryRequestResults       :: [InlineQueryResult] -- ^ A JSON-serialized array of results for the inline query.
  , answerInlineQueryCacheTime            :: Maybe Seconds       -- ^ The maximum amount of time in seconds that the result of the inline query may be cached on the server. Defaults to 300.
  , answerInlineQueryIsPersonal           :: Maybe Bool          -- ^ Pass 'True', if results may be cached on the server side only for the user that sent the query. By default, results may be returned to any user who sends the same query.
  , answerInlineQueryNextOffset           :: Maybe Text          -- ^ Pass the offset that a client should send in the next query with the same text to receive more results. Pass an empty string if there are no more results or if you don't support pagination. Offset length can't exceed 64 bytes.
  , answerInlineQueryButton               :: Maybe InlineQueryResultsButton -- ^ A JSON-serialized object describing a button to be shown above inline query results.
  } deriving (Generic)

instance ToJSON AnswerInlineQueryRequest where toJSON = gtoJSON
instance FromJSON AnswerInlineQueryRequest where parseJSON = gparseJSON

data ChosenInlineResult = ChosenInlineResult
  { chosenInlineResultResultId        :: InlineQueryResultId -- ^ The unique identifier for the result that was chosen.
  , chosenInlineResultFrom            :: User            -- ^ The user that chose the result.
  , chosenInlineResultLocation        :: Maybe Location  -- ^ Sender location, only for bots that require user location.
  , chosenInlineResultInlineMessageId :: Maybe MessageId -- ^ Identifier of the sent inline message. Available only if there is an inline keyboard attached to the message. Will be also received in callback queries and can be used to edit the message.
  , chosenInlineResultQuery           :: InlineQueryId   -- ^ The query that was used to obtain the result.
  } deriving (Generic, Show)

instance ToJSON ChosenInlineResult where toJSON = gtoJSON
instance FromJSON ChosenInlineResult where parseJSON = gparseJSON

deriveJSON' ''InlineQuery

makeDefault ''AnswerInlineQueryRequest
