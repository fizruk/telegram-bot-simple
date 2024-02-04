{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetMessageReaction where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'setMessageReaction'

-- | Request parameters for 'setMessageReaction'
data SetMessageReactionRequest = SetMessageReactionRequest
  { setMessageReactionRequestChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format \@channelusername).
  , setMessageReactionRequestMessageId :: MessageId -- ^ Identifier of the target message. If the message belongs to a media group, the reaction is set to the first non-deleted message in the group instead.
  , setMessageReactionRequestReaction :: Maybe [ReactionType] -- ^ New list of reaction types to set on the message. Currently, as non-premium users, bots can set up to one reaction per message. A custom emoji reaction can be used if it is either already present on the message or explicitly allowed by chat administrators.
  , setMessageReactionRequestIsBig :: Maybe Bool -- ^ Pass 'True' to set the reaction with a big animation.
  }
  deriving Generic

instance ToJSON   SetMessageReactionRequest where toJSON = gtoJSON
instance FromJSON SetMessageReactionRequest where parseJSON = gparseJSON

type SetMessageReaction = "setMessageReaction"
  :> ReqBody '[JSON] SetMessageReactionRequest
  :> Post '[JSON] (Response Bool)

setMessageReaction :: SetMessageReactionRequest -> ClientM (Response Bool)
setMessageReaction = client (Proxy @SetMessageReaction)

makeDefault ''SetMessageReactionRequest
