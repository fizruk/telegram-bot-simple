{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Telegram.Bot.Simple.RunTG where

import Telegram.Bot.API
import Data.Text
import Servant.Client hiding (Response)
import Telegram.Bot.Simple.Eff (liftClientM, BotM)

-- | A preferrible way to run telegram requests
class RunTG a b | a -> b where
  runTG :: a -> BotM b

instance RunTG (ClientM (Response a)) (Response a) where
  runTG = liftClientM

instance RunTG AnswerInlineQueryRequest (Response Bool) where
  runTG = runTG . answerInlineQuery

instance RunTG AnswerWebAppQueryRequest (Response SentWebAppMessage) where
  runTG = runTG . answerWebAppQuery

instance RunTG SendInvoiceRequest (Response Message) where
  runTG = runTG . sendInvoice

instance RunTG CreateInvoiceLinkRequest (Response Text) where
  runTG = runTG . createInvoiceLink

instance RunTG AnswerShippingQueryRequest (Response Bool) where
  runTG = runTG . answerShippingQuery

instance RunTG AnswerPreCheckoutQueryRequest (Response Bool) where
  runTG = runTG . answerPreCheckoutQuery

instance RunTG GetUpdatesRequest (Response [Update]) where
  runTG = runTG . getUpdates

instance RunTG SendGameRequest (Response Message) where
  runTG = runTG . sendGame

instance RunTG SetGameScoreRequest (Response SetGameScoreResult) where
  runTG = runTG . setGameScore

instance RunTG SendStickerRequest (Response Message) where
  runTG = runTG . sendSticker

instance RunTG UploadStickerFileRequest (Response File) where
  runTG = runTG . uploadStickerFile

instance RunTG CreateNewStickerSetRequest (Response Bool) where
  runTG = runTG . createNewStickerSet

instance RunTG AddStickerToSetRequest (Response Bool) where
  runTG = runTG . addStickerToSet

instance RunTG SetStickerSetThumbRequest (Response Bool) where
  runTG = runTG . setStickerSetThumb

instance RunTG EditMessageTextRequest (Response EditMessageResponse) where
  runTG = runTG . editMessageText

instance RunTG EditMessageCaptionRequest (Response EditMessageResponse) where
  runTG = runTG . editMessageCaption

instance RunTG EditMessageMediaRequest (Response EditMessageResponse) where
  runTG = runTG . editMessageMedia

instance RunTG EditMessageReplyMarkupRequest (Response EditMessageResponse) where
  runTG = runTG . editMessageReplyMarkup

instance RunTG StopPollRequest (Response Poll) where
  runTG = runTG . stopPoll

instance RunTG CreateForumTopicRequest (Response ForumTopic) where
  runTG = runTG . createForumTopic

instance RunTG EditForumTopicRequest (Response Bool) where
  runTG = runTG . editForumTopic

instance RunTG CloseForumTopicRequest (Response Bool) where
  runTG = runTG . closeForumTopic

instance RunTG ReopenForumTopicRequest (Response Bool) where
  runTG = runTG . reopenForumTopic

instance RunTG DeleteForumTopicRequest (Response Bool) where
  runTG = runTG . deleteForumTopic

instance RunTG UnpinAllForumTopicMessagesRequest (Response Bool) where
  runTG = runTG . unpinAllForumTopicMessages

instance RunTG EditGeneralForumTopicRequest (Response Bool) where
  runTG = runTG . editGeneralForumTopic

instance RunTG CloseGeneralForumTopicRequest (Response Bool) where
  runTG = runTG . closeGeneralForumTopic

instance RunTG ReopenGeneralForumTopicRequest (Response Bool) where
  runTG = runTG . reopenGeneralForumTopic

instance RunTG HideGeneralForumTopicRequest (Response Bool) where
  runTG = runTG . hideGeneralForumTopic

instance RunTG UnhideGeneralForumTopicRequest (Response Bool) where
  runTG = runTG . unhideGeneralForumTopic

instance RunTG ForwardMessageRequest (Response Message) where
  runTG = runTG . forwardMessage

instance RunTG SendDiceRequest (Response Message) where
  runTG = runTG . sendDice

instance RunTG UnbanChatMemberRequest (Response Bool) where
  runTG = runTG . unbanChatMember

instance RunTG SendLocationRequest (Response Message) where
  runTG = runTG . sendLocation

instance RunTG SendVoiceRequest (Response Message) where
  runTG = runTG . sendVoice

instance RunTG SendAudioRequest (Response Message) where
  runTG = runTG . sendAudio

instance RunTG SendVideoRequest (Response Message) where
  runTG = runTG . sendVideo

instance RunTG SetChatPhotoRequest (Response Bool) where
  runTG = runTG . setChatPhoto

instance RunTG DeleteMyCommandsRequest (Response Bool) where
  runTG = runTG . deleteMyCommands

instance RunTG EditMessageLiveLocationRequest (Response (Either Bool Message)) where
  runTG = runTG . editMessageLiveLocation

instance RunTG SetChatMenuButtonRequest (Response Bool) where
  runTG = runTG . setChatMenuButton

instance RunTG SetMyCommandsRequest (Response Bool) where
  runTG = runTG . setMyCommands

instance RunTG CopyMessageRequest (Response CopyMessageId) where
  runTG = runTG . copyMessage

instance RunTG SendMessageRequest (Response Message) where
  runTG = runTG . sendMessage

instance RunTG EditChatInviteLinkRequest (Response ChatInviteLink) where
  runTG = runTG . editChatInviteLink

instance RunTG SendPhotoRequest (Response Message) where
  runTG = runTG . sendPhoto

instance RunTG StopMessageLiveLocationRequest (Response (Either Bool Message)) where
  runTG = runTG . stopMessageLiveLocation

instance RunTG SendDocumentRequest (Response Message) where
  runTG = runTG . sendDocument

instance RunTG SendAnimationRequest (Response Message) where
  runTG = runTG . sendAnimation

instance RunTG RestrictChatMemberRequest (Response Bool) where
  runTG = runTG . restrictChatMember

instance RunTG AnswerCallbackQueryRequest (Response Bool) where
  runTG = runTG . answerCallbackQuery

instance RunTG SetMyDefaultAdministratorRightsRequest (Response Bool) where
  runTG = runTG . setMyDefaultAdministratorRights

instance RunTG CreateChatInviteLinkRequest (Response ChatInviteLink) where
  runTG = runTG . createChatInviteLink

instance RunTG PinChatMessageRequest (Response Bool) where
  runTG = runTG . pinChatMessage

instance RunTG SetChatPermissionsRequest (Response Bool) where
  runTG = runTG . setChatPermissions

instance RunTG PromoteChatMemberRequest (Response Bool) where
  runTG = runTG . promoteChatMember

instance RunTG GetMyDefaultAdministratorRightsRequest (Response ChatAdministratorRights) where
  runTG = runTG . getMyDefaultAdministratorRights

instance RunTG BanChatMemberRequest (Response Bool) where
  runTG = runTG . banChatMember

instance RunTG GetChatMenuButtonRequest (Response MenuButton) where
  runTG = runTG . getChatMenuButton

instance RunTG SendPollRequest (Response Message) where
  runTG = runTG . sendPoll

instance RunTG GetMyCommandsRequest (Response [BotCommand]) where
  runTG = runTG . getMyCommands

instance RunTG SendVenueRequest (Response Message) where
  runTG = runTG . sendVenue

instance RunTG SendMediaGroupRequest (Response [Message]) where
  runTG = runTG . sendMediaGroup

instance RunTG SetChatAdministratorCustomTitleRequest (Response Bool) where
  runTG = runTG . setChatAdministratorCustomTitle

instance RunTG SendVideoNoteRequest (Response Message) where
  runTG = runTG . sendVideoNote

instance RunTG SendContactRequest (Response Message) where
  runTG = runTG . sendContact

instance RunTG GetUserProfilePhotosRequest (Response UserProfilePhotos) where
  runTG = runTG . getUserProfilePhotos
