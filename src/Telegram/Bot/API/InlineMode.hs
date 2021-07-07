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
import           Servant.Multipart

import           Telegram.Bot.API.Internal.Utils
import           Telegram.Bot.API.MakingRequests
import           Telegram.Bot.API.Types
import           Telegram.Bot.API.InlineMode.InlineQueryResult
import           Telegram.Bot.API.InlineMode.InputMessageContent
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
  { answerInlineQueryRequestInlineQueryId :: InlineQueryId
  , answerInlineQueryRequestResults       :: [InlineQueryResult]
  } deriving (Generic)

instance ToJSON AnswerInlineQueryRequest where toJSON = gtoJSON
instance FromJSON AnswerInlineQueryRequest where parseJSON = gparseJSON

deriveJSON' ''InlineQuery
