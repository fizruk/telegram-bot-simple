{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SendPoll where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Types.ParseMode
import Telegram.Bot.API.Internal.TH

-- ** 'sendPoll'

-- | Request parameters for 'sendPoll'.
data SendPollRequest = SendPollRequest
  { sendPollBusinessConnectionId :: Maybe BusinessConnectionId -- ^ Unique identifier of the business connection on behalf of which the message will be sent.
  , sendPollChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendPollMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendPollQuestion :: Text -- ^ Poll question, 1-300 characters.
  , sendPollQuestionParseMode :: Maybe ParseMode -- ^ Mode for parsing entities in the question. See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details. Currently, only custom emoji entities are allowed.
  , sendPollQuestionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the poll question. It can be specified instead of @question_parse_mode@.
  , sendPollOptions :: [InputPollOption] -- ^ A JSON-serialized list of answer options, 2-10 strings 1-100 characters each.
  , sendPollIsAnonymous :: Maybe Bool -- ^ True, if the poll needs to be anonymous, defaults to 'True'.
  , sendPollType :: Maybe Text -- ^ Poll type, “quiz” or “regular”, defaults to “regular”.
  , sendPollAllowsMultipleAnswers :: Maybe Bool -- ^ True, if the poll allows multiple answers, ignored for polls in quiz mode, defaults to 'False'.
  , sendPollCorrectOptionId :: Maybe Int -- ^ 0-based identifier of the correct answer option, required for polls in quiz mode.
  , sendPollExplanation :: Maybe Text -- ^ Text that is shown when a user chooses an incorrect answer or taps on the lamp icon in a quiz-style poll, 0-200 characters with at most 2 line feeds after entities parsing.
  , sendPollExplanationParseMode :: Maybe ParseMode  -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , sendPollExplanationEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the poll explanation, which can be specified instead of @parse_mode@.
  , sendPollOpenPeriod :: Maybe Int -- ^ Amount of time in seconds the poll will be active after creation, 5-600. Can't be used together with @close_date@.
  , sendPollCloseDate :: Maybe Int -- ^ Point in time (Unix timestamp) when the poll will be automatically closed. Must be at least 5 and no more than 600 seconds in the future. Can't be used together with @open_period@.
  , sendPollIsClosed :: Maybe Bool -- ^ Pass True, if the poll needs to be immediately closed. This can be useful for poll preview.
  , sendPollDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendPollProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , sendPollMessageEffectId :: Maybe Text -- ^ Unique identifier of the message effect to be added to the message; for private chats only.
  , sendPollReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  , sendPollReplyParameters :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , sendPollReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON   SendPollRequest where toJSON = gtoJSON
instance FromJSON SendPollRequest where parseJSON = gparseJSON

type SendPoll = "sendPoll"
  :> ReqBody '[JSON] SendPollRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send a native poll.
--   On success, the sent Message is returned.
sendPoll :: SendPollRequest ->  ClientM (Response Message)
sendPoll = client (Proxy @SendPoll)

makeDefault ''SendPollRequest
