{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module Telegram.Bot.API.WebApps where

import Data.Text (Text)
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils (deriveJSON')
import Telegram.Bot.API.MakingRequests (Response)
import Telegram.Bot.API.Types.Common (InlineMessageId)
import Telegram.Bot.API.InlineMode.InlineQueryResult (InlineQueryResult)
import Telegram.Bot.API.Internal.TH (makeDefault)

-- * Types

-- ** 'AnswerWebAppQueryRequest'

data AnswerWebAppQueryRequest = AnswerWebAppQueryRequest
  { answerWebAppQueryWebAppQueryId :: Text              -- ^ Unique identifier for the query to be answered.
  , answerWebAppQueryResult        :: InlineQueryResult -- ^ A JSON-serialized object describing the message to be sent.
  }
  deriving (Generic, Show)

-- ** 'SentWebAppMessage'

-- | Contains information about an inline message sent by a Web App on behalf of a user.
newtype SentWebAppMessage = SentWebAppMessage
  { sentWebAppMessageInlineMessageId :: Maybe InlineMessageId
  }
  deriving (Generic, Show)

foldMap deriveJSON'
  [ ''SentWebAppMessage
  , ''AnswerWebAppQueryRequest
  ]

-- * Methods

-- ** 'answerWebAppQuery'

type AnswerWebAppQuery
  = "answerWebAppQuery" :> ReqBody '[JSON] AnswerWebAppQueryRequest :> Post '[JSON] (Response SentWebAppMessage)

-- | Use this method to set the result of an interaction with a Web App
-- and send a corresponding message on behalf of the user
-- to the chat from which the query originated.
--
-- On success, a 'SentWebAppMessage' object is returned.
answerWebAppQuery :: AnswerWebAppQueryRequest -> ClientM (Response SentWebAppMessage)
answerWebAppQuery = client (Proxy @AnswerWebAppQuery)

foldMap makeDefault
  [ ''SentWebAppMessage
  , ''AnswerWebAppQueryRequest
  ]
