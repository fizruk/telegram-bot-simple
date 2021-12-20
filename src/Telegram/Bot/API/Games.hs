{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module Telegram.Bot.API.Games where

import Data.Text (Text)
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils (deriveJSON')
import Telegram.Bot.API.MakingRequests (Response)
import Telegram.Bot.API.Types (ChatId, GameHighScore, InlineKeyboardMarkup, Message, MessageId, UserId)

-- * Types

-- ** 'SendGameRequest'

data SendGameRequest = SendGameRequest
  { sendGameRequestChatId                   :: ChatId                     -- ^ Unique identifier for the target chat.
  , sendGameRequestGameShortName            :: Text                       -- ^ Short name of the game, serves as the unique identifier for the game. Set up your games via Botfather.
  , sendGameRequestDisableNotification      :: Maybe Bool                 -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendGameRequestReplyToMessageId         :: Maybe MessageId            -- ^ If the message is a reply, ID of the original message.
  , sendGameRequestAllowSendingWithoutReply :: Maybe Bool                 -- ^ Pass 'True', if the message should be sent even if the specified replied-to message is not found
  , sendGameRequestReplyMarkup              :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard. If empty, one 'Play game_title' button will be shown. If not empty, the first button must launch the game.
  }
  deriving (Generic, Show)

-- ** 'SetGameScoreRequest'

data SetGameScoreRequest = SetGameScoreRequest
  { setGameScoreRequestUserId             :: UserId          -- ^ User identifier.
  , setGameScoreRequestScore              :: Integer         -- ^ New score, must be non-negative.
  , setGameScoreRequestForce              :: Maybe Bool      -- ^ Pass 'True', if the high score is allowed to decrease. This can be useful when fixing mistakes or banning cheaters.
  , setGameScoreRequestDisableEditMessage :: Maybe Bool      -- ^ Pass 'True', if the game message should not be automatically edited to include the current scoreboard.
  , setGameScoreRequestChatId             :: Maybe ChatId    -- ^ Required if @inline_message_id@ is not specified. Unique identifier for the target chat
  , setGameScoreRequestMessageId          :: Maybe MessageId -- ^ Required if @inline_message_id@ is not specified. Identifier of the sent message.
  , setGameScoreRequestInlineMessageId    :: Maybe MessageId -- ^ Required if @chat_id@ and @message_id@ are not specified. Identifier of the inline message.
  }
  deriving (Generic, Show)

-- ** 'SetGameScoreResult'

data SetGameScoreResult = SetGameScoreMessage Message | SetGameScoreMessageBool Bool
  deriving (Generic, Show)

-- ** 'GetGameHighScoresRequest'

data GetGameHighScoresRequest = GetGameHighScoresRequest
  { getGameHighScoresRequestUserId          :: UserId          -- ^ Target user id.
  , getGameHighScoresRequestChatId          :: Maybe ChatId    -- ^ Required if @inline_message_id@ is not specified. Unique identifier for the target chat.
  , getGameHighScoresRequestMessageId       :: Maybe MessageId -- ^ Required if @inline_message_id@ is not specified. Identifier of the sent message.
  , getGameHighScoresRequestInlineMessageId :: Maybe MessageId -- ^ Required if @chat_id@ and @message_id@ are not specified. Identifier of the inline message.
  }
  deriving (Generic, Show)

-- * Methods

-- ** 'sendGame'

type SendGame
  = "sendGame" :> ReqBody '[JSON] SendGameRequest :> Post '[JSON] (Response Message)

-- | Use this method to send a game. On success, the sent 'Message' is returned.
sendGame :: SendGameRequest -> ClientM (Response Message)
sendGame = client (Proxy @SendGame)

-- ** 'setGameScore'

type SetGameScore
  = "setGameScore" :> ReqBody '[JSON] SetGameScoreRequest :> Post '[JSON] (Response SetGameScoreResult)

-- | Use this method to set the score of the specified user in a game message. On success, if the message is not an inline message, the 'Message' is returned, otherwise True is returned. Returns an error, if the new score is not greater than the user's current score in the chat and force is False.
setGameScore :: SetGameScoreRequest -> ClientM (Response SetGameScoreResult)
setGameScore = client (Proxy @SetGameScore)

-- ** 'getGameHighScores'

type GetGameHighScores
  = "getGameHighScores" :> ReqBody '[JSON] GetGameHighScoresRequest :> Post '[JSON] (Response [GameHighScore])


foldMap deriveJSON'
  [ ''SendGameRequest
  , ''SetGameScoreRequest
  , ''SetGameScoreResult
  ]
