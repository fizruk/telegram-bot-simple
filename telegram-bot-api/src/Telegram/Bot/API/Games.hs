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
import Telegram.Bot.API.Types
  ( BusinessConnectionId, ChatId, GameHighScore, InlineKeyboardMarkup
  , Message, MessageId, MessageThreadId
  , ReplyParameters, UserId
  )
import Telegram.Bot.API.Internal.TH

-- * Types

-- ** 'SendGameRequest'

data SendGameRequest = SendGameRequest
  { sendGameBusinessConnectionId     :: Maybe BusinessConnectionId -- ^ Unique identifier of the business connection on behalf of which the message will be sent.
  , sendGameChatId                   :: ChatId                     -- ^ Unique identifier for the target chat.
  , sendGameMessageThreadId          :: Maybe MessageThreadId      -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendGameGameShortName            :: Text                       -- ^ Short name of the game, serves as the unique identifier for the game. Set up your games via Botfather.
  , sendGameDisableNotification      :: Maybe Bool                 -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendGameProtectContent           :: Maybe Bool                 -- ^ Protects the contents of the sent message from forwarding and saving.
  , sendGameReplyToMessageId         :: Maybe MessageId            -- ^ If the message is a reply, ID of the original message.
  , sendGameReplyParameters          :: Maybe ReplyParameters      -- ^ Description of the message to reply to.
  , sendGameReplyMarkup              :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard. If empty, one 'Play game_title' button will be shown. If not empty, the first button must launch the game.
  }
  deriving (Generic, Show)

-- ** 'SetGameScoreRequest'

data SetGameScoreRequest = SetGameScoreRequest
  { setGameScoreUserId             :: UserId          -- ^ User identifier.
  , setGameScoreScore              :: Integer         -- ^ New score, must be non-negative.
  , setGameScoreForce              :: Maybe Bool      -- ^ Pass 'True', if the high score is allowed to decrease. This can be useful when fixing mistakes or banning cheaters.
  , setGameScoreDisableEditMessage :: Maybe Bool      -- ^ Pass 'True', if the game message should not be automatically edited to include the current scoreboard.
  , setGameScoreChatId             :: Maybe ChatId    -- ^ Required if @inline_message_id@ is not specified. Unique identifier for the target chat
  , setGameScoreMessageId          :: Maybe MessageId -- ^ Required if @inline_message_id@ is not specified. Identifier of the sent message.
  , setGameScoreInlineMessageId    :: Maybe MessageId -- ^ Required if @chat_id@ and @message_id@ are not specified. Identifier of the inline message.
  }
  deriving (Generic, Show)

-- ** 'SetGameScoreResult'

data SetGameScoreResult = SetGameScoreMessage Message | SetGameScoreMessageBool Bool
  deriving (Generic, Show)

-- ** 'GetGameHighScoresRequest'

data GetGameHighScoresRequest = GetGameHighScoresRequest
  { getGameHighScoresUserId          :: UserId          -- ^ Target user id.
  , getGameHighScoresChatId          :: Maybe ChatId    -- ^ Required if @inline_message_id@ is not specified. Unique identifier for the target chat.
  , getGameHighScoresMessageId       :: Maybe MessageId -- ^ Required if @inline_message_id@ is not specified. Identifier of the sent message.
  , getGameHighScoresInlineMessageId :: Maybe MessageId -- ^ Required if @chat_id@ and @message_id@ are not specified. Identifier of the inline message.
  }
  deriving (Generic, Show)

foldMap deriveJSON'
  [ ''SendGameRequest
  , ''SetGameScoreRequest
  , ''SetGameScoreResult
  ]

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

foldMap makeDefault
  [ ''SendGameRequest
  , ''SetGameScoreRequest
  , ''GetGameHighScoresRequest
  ]
