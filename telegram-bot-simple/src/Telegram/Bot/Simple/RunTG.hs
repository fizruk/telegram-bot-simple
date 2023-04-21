{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Telegram.Bot.Simple.RunTG where

import Telegram.Bot.API
import Data.Text
import Servant.Client hiding (Response)
import Telegram.Bot.Simple.Eff (liftClientM, BotM)

-- * 'RunTG'

-- | The most preferrable way to run telegram requests.
--
-- E.g. instead of invoking @liftClientM $ methodName MethodNameRequest {..}@, you just need to specify @runTG $ defMethodName params@. See @examples@ for more details.
class RunTG a b | a -> b where
  runTG :: a -> BotM b

-- ** Instances

-- | A servant client associated with a response type. Alias for 'liftClientM'.
instance RunTG (ClientM (Response a)) (Response a) where
  runTG = liftClientM

-- | Wrapper around 'AnswerInlineQueryRequest' request type for 'answerInlineQuery' method.
instance RunTG AnswerInlineQueryRequest (Response Bool) where
  runTG = runTG . answerInlineQuery

-- | Wrapper around 'AnswerWebAppQueryRequest' request type for 'answerWebAppQuery' method.
instance RunTG AnswerWebAppQueryRequest (Response SentWebAppMessage) where
  runTG = runTG . answerWebAppQuery

-- | Wrapper around 'SendInvoiceRequest' request type for 'sendInvoice' method.
instance RunTG SendInvoiceRequest (Response Message) where
  runTG = runTG . sendInvoice

-- | Wrapper around 'CreateInvoiceLinkRequest' request type for 'createInvoiceLink' method.
instance RunTG CreateInvoiceLinkRequest (Response Text) where
  runTG = runTG . createInvoiceLink

-- | Wrapper around 'AnswerShippingQueryRequest' request type for 'answerShippingQuery' method.
instance RunTG AnswerShippingQueryRequest (Response Bool) where
  runTG = runTG . answerShippingQuery

-- | Wrapper around 'AnswerPreCheckoutQueryRequest' request type for 'answerPreCheckoutQuery' method.
instance RunTG AnswerPreCheckoutQueryRequest (Response Bool) where
  runTG = runTG . answerPreCheckoutQuery

-- | Wrapper around 'GetUpdatesRequest' request type for 'getUpdates' method.
instance RunTG GetUpdatesRequest (Response [Update]) where
  runTG = runTG . getUpdates

-- | Wrapper around 'SendGameRequest' request type for 'sendGame' method.
instance RunTG SendGameRequest (Response Message) where
  runTG = runTG . sendGame

-- | Wrapper around 'SetGameScoreRequest' request type for 'setGameScore' method.
instance RunTG SetGameScoreRequest (Response SetGameScoreResult) where
  runTG = runTG . setGameScore

-- | Wrapper around 'SendStickerRequest' request type for 'sendSticker' method.
instance RunTG SendStickerRequest (Response Message) where
  runTG = runTG . sendSticker

-- | Wrapper around 'UploadStickerFileRequest' request type for 'uploadStickerFile' method.
instance RunTG UploadStickerFileRequest (Response File) where
  runTG = runTG . uploadStickerFile

-- | Wrapper around 'CreateNewStickerSetRequest' request type for 'createNewStickerSet' method.
instance RunTG CreateNewStickerSetRequest (Response Bool) where
  runTG = runTG . createNewStickerSet

-- | Wrapper around 'AddStickerToSetRequest' request type for 'addStickerToSet' method.
instance RunTG AddStickerToSetRequest (Response Bool) where
  runTG = runTG . addStickerToSet

-- | Wrapper around 'SetStickerSetThumbRequest' request type for 'setStickerSetThumb' method.
instance RunTG SetStickerSetThumbnailRequest (Response Bool) where
  runTG = runTG . setStickerSetThumbnail

-- | Wrapper around 'EditMessageTextRequest' request type for 'editMessageText' method.
instance RunTG EditMessageTextRequest (Response EditMessageResponse) where
  runTG = runTG . editMessageText

-- | Wrapper around 'EditMessageCaptionRequest' request type for 'editMessageCaption' method.
instance RunTG EditMessageCaptionRequest (Response EditMessageResponse) where
  runTG = runTG . editMessageCaption

-- | Wrapper around 'EditMessageMediaRequest' request type for 'editMessageMedia' method.
instance RunTG EditMessageMediaRequest (Response EditMessageResponse) where
  runTG = runTG . editMessageMedia

-- | Wrapper around 'EditMessageReplyMarkupRequest' request type for 'editMessageReplyMarkup' method.
instance RunTG EditMessageReplyMarkupRequest (Response EditMessageResponse) where
  runTG = runTG . editMessageReplyMarkup

-- | Wrapper around 'StopPollRequest' request type for 'stopPoll' method.
instance RunTG StopPollRequest (Response Poll) where
  runTG = runTG . stopPoll

-- | Wrapper around 'CreateForumTopicRequest' request type for 'createForumTopic' method.
instance RunTG CreateForumTopicRequest (Response ForumTopic) where
  runTG = runTG . createForumTopic

-- | Wrapper around 'EditForumTopicRequest' request type for 'editForumTopic' method.
instance RunTG EditForumTopicRequest (Response Bool) where
  runTG = runTG . editForumTopic

-- | Wrapper around 'CloseForumTopicRequest' request type for 'closeForumTopic' method.
instance RunTG CloseForumTopicRequest (Response Bool) where
  runTG = runTG . closeForumTopic

-- | Wrapper around 'ReopenForumTopicRequest' request type for 'reopenForumTopic' method.
instance RunTG ReopenForumTopicRequest (Response Bool) where
  runTG = runTG . reopenForumTopic

-- | Wrapper around 'DeleteForumTopicRequest' request type for 'deleteForumTopic' method.
instance RunTG DeleteForumTopicRequest (Response Bool) where
  runTG = runTG . deleteForumTopic

-- | Wrapper around 'UnpinAllForumTopicMessagesRequest' request type for 'unpinAllForumTopicMessages' method.
instance RunTG UnpinAllForumTopicMessagesRequest (Response Bool) where
  runTG = runTG . unpinAllForumTopicMessages

-- | Wrapper around 'EditGeneralForumTopicRequest' request type for 'editGeneralForumTopic' method.
instance RunTG EditGeneralForumTopicRequest (Response Bool) where
  runTG = runTG . editGeneralForumTopic

-- | Wrapper around 'CloseGeneralForumTopicRequest' request type for 'closeGeneralForumTopic' method.
instance RunTG CloseGeneralForumTopicRequest (Response Bool) where
  runTG = runTG . closeGeneralForumTopic

-- | Wrapper around 'ReopenGeneralForumTopicRequest' request type for 'reopenGeneralForumTopic' method.
instance RunTG ReopenGeneralForumTopicRequest (Response Bool) where
  runTG = runTG . reopenGeneralForumTopic

-- | Wrapper around 'HideGeneralForumTopicRequest' request type for 'hideGeneralForumTopic' method.
instance RunTG HideGeneralForumTopicRequest (Response Bool) where
  runTG = runTG . hideGeneralForumTopic

-- | Wrapper around 'UnhideGeneralForumTopicRequest' request type for 'unhideGeneralForumTopic' method.
instance RunTG UnhideGeneralForumTopicRequest (Response Bool) where
  runTG = runTG . unhideGeneralForumTopic

-- | Wrapper around 'ForwardMessageRequest' request type for 'forwardMessage' method.
instance RunTG ForwardMessageRequest (Response Message) where
  runTG = runTG . forwardMessage

-- | Wrapper around 'SendDiceRequest' request type for 'sendDice' method.
instance RunTG SendDiceRequest (Response Message) where
  runTG = runTG . sendDice

-- | Wrapper around 'UnbanChatMemberRequest' request type for 'unbanChatMember' method.
instance RunTG UnbanChatMemberRequest (Response Bool) where
  runTG = runTG . unbanChatMember

-- | Wrapper around 'SendLocationRequest' request type for 'sendLocation' method.
instance RunTG SendLocationRequest (Response Message) where
  runTG = runTG . sendLocation

-- | Wrapper around 'SendVoiceRequest' request type for 'sendVoice' method.
instance RunTG SendVoiceRequest (Response Message) where
  runTG = runTG . sendVoice

-- | Wrapper around 'SendAudioRequest' request type for 'sendAudio' method.
instance RunTG SendAudioRequest (Response Message) where
  runTG = runTG . sendAudio

-- | Wrapper around 'SendVideoRequest' request type for 'sendVideo' method.
instance RunTG SendVideoRequest (Response Message) where
  runTG = runTG . sendVideo

-- | Wrapper around 'SetChatPhotoRequest' request type for 'setChatPhoto' method.
instance RunTG SetChatPhotoRequest (Response Bool) where
  runTG = runTG . setChatPhoto

-- | Wrapper around 'DeleteMyCommandsRequest' request type for 'deleteMyCommands' method.
instance RunTG DeleteMyCommandsRequest (Response Bool) where
  runTG = runTG . deleteMyCommands

-- | Wrapper around 'EditMessageLiveLocationRequest' request type for 'editMessageLiveLocation' method.
instance RunTG EditMessageLiveLocationRequest (Response (Either Bool Message)) where
  runTG = runTG . editMessageLiveLocation

-- | Wrapper around 'SetChatMenuButtonRequest' request type for 'setChatMenuButton' method.
instance RunTG SetChatMenuButtonRequest (Response Bool) where
  runTG = runTG . setChatMenuButton

-- | Wrapper around 'SetMyCommandsRequest' request type for 'setMyCommands' method.
instance RunTG SetMyCommandsRequest (Response Bool) where
  runTG = runTG . setMyCommands

instance RunTG SetCustomEmojiStickerSetThumbnailRequest (Response Bool) where
  runTG = runTG . setCustomEmojiStickerSetThumbnail

-- | Wrapper around 'CopyMessageRequest' request type for 'copyMessage' method.
instance RunTG CopyMessageRequest (Response CopyMessageId) where
  runTG = runTG . copyMessage

-- | Wrapper around 'SendMessageRequest' request type for 'sendMessage' method.
instance RunTG SendMessageRequest (Response Message) where
  runTG = runTG . sendMessage

-- | Wrapper around 'EditChatInviteLinkRequest' request type for 'editChatInviteLink' method.
instance RunTG EditChatInviteLinkRequest (Response ChatInviteLink) where
  runTG = runTG . editChatInviteLink

-- | Wrapper around 'SendPhotoRequest' request type for 'sendPhoto' method.
instance RunTG SendPhotoRequest (Response Message) where
  runTG = runTG . sendPhoto

-- | Wrapper around 'StopMessageLiveLocationRequest' request type for 'stopMessageLiveLocation' method.
instance RunTG StopMessageLiveLocationRequest (Response (Either Bool Message)) where
  runTG = runTG . stopMessageLiveLocation

-- | Wrapper around 'SendDocumentRequest' request type for 'sendDocument' method.
instance RunTG SendDocumentRequest (Response Message) where
  runTG = runTG . sendDocument

-- | Wrapper around 'SendAnimationRequest' request type for 'sendAnimation' method.
instance RunTG SendAnimationRequest (Response Message) where
  runTG = runTG . sendAnimation

-- | Wrapper around 'RestrictChatMemberRequest' request type for 'restrictChatMember' method.
instance RunTG RestrictChatMemberRequest (Response Bool) where
  runTG = runTG . restrictChatMember

-- | Wrapper around 'AnswerCallbackQueryRequest' request type for 'answerCallbackQuery' method.
instance RunTG AnswerCallbackQueryRequest (Response Bool) where
  runTG = runTG . answerCallbackQuery

-- | Wrapper around 'SetMyDefaultAdministratorRightsRequest' request type for 'setMyDefaultAdministratorRights' method.
instance RunTG SetMyDefaultAdministratorRightsRequest (Response Bool) where
  runTG = runTG . setMyDefaultAdministratorRights

-- | Wrapper around 'SetMyDescriptionRequest' request type for 'setMyDescription' method.
instance RunTG SetMyDescriptionRequest (Response Bool) where
  runTG = runTG . setMyDescription

-- | Wrapper around 'SetMyShortDescriptionRequest' request type for 'setMyShortDescription' method.
instance RunTG SetMyShortDescriptionRequest (Response Bool) where
  runTG = runTG . setMyShortDescription

-- | Wrapper around 'CreateChatInviteLinkRequest' request type for 'createChatInviteLink' method.
instance RunTG CreateChatInviteLinkRequest (Response ChatInviteLink) where
  runTG = runTG . createChatInviteLink

-- | Wrapper around 'PinChatMessageRequest' request type for 'pinChatMessage' method.
instance RunTG PinChatMessageRequest (Response Bool) where
  runTG = runTG . pinChatMessage

-- | Wrapper around 'SetChatPermissionsRequest' request type for 'setChatPermissions' method.
instance RunTG SetChatPermissionsRequest (Response Bool) where
  runTG = runTG . setChatPermissions

-- | Wrapper around 'PromoteChatMemberRequest' request type for 'promoteChatMember' method.
instance RunTG PromoteChatMemberRequest (Response Bool) where
  runTG = runTG . promoteChatMember

-- | Wrapper around 'GetMyDefaultAdministratorRightsRequest' request type for 'getMyDefaultAdministratorRights' method.
instance RunTG GetMyDefaultAdministratorRightsRequest (Response ChatAdministratorRights) where
  runTG = runTG . getMyDefaultAdministratorRights

-- | Wrapper around 'GetMyDescriptionRequest' request type for 'getMyDescription' method.
instance RunTG GetMyDescriptionRequest (Response BotDescription) where
  runTG = runTG . getMyDescription

-- | Wrapper around 'GetMyShortDescriptionRequest' request type for 'getMyShortDescription' method.
instance RunTG GetMyShortDescriptionRequest (Response BotShortDescription) where
  runTG = runTG . getMyShortDescription

-- | Wrapper around 'BanChatMemberRequest' request type for 'banChatMember' method.
instance RunTG BanChatMemberRequest (Response Bool) where
  runTG = runTG . banChatMember

-- | Wrapper around 'GetChatMenuButtonRequest' request type for 'getChatMenuButton' method.
instance RunTG GetChatMenuButtonRequest (Response MenuButton) where
  runTG = runTG . getChatMenuButton

-- | Wrapper around 'SendPollRequest' request type for 'sendPoll' method.
instance RunTG SendPollRequest (Response Message) where
  runTG = runTG . sendPoll

-- | Wrapper around 'GetMyCommandsRequest' request type for 'getMyCommands' method.
instance RunTG GetMyCommandsRequest (Response [BotCommand]) where
  runTG = runTG . getMyCommands

-- | Wrapper around 'SendVenueRequest' request type for 'sendVenue' method.
instance RunTG SendVenueRequest (Response Message) where
  runTG = runTG . sendVenue

-- | Wrapper around 'SendMediaGroupRequest' request type for 'sendMediaGroup' method.
instance RunTG SendMediaGroupRequest (Response [Message]) where
  runTG = runTG . sendMediaGroup

-- | Wrapper around 'SetChatAdministratorCustomTitleRequest' request type for 'setChatAdministratorCustomTitle' method.
instance RunTG SetChatAdministratorCustomTitleRequest (Response Bool) where
  runTG = runTG . setChatAdministratorCustomTitle

-- | Wrapper around 'SendVideoNoteRequest' request type for 'sendVideoNote' method.
instance RunTG SendVideoNoteRequest (Response Message) where
  runTG = runTG . sendVideoNote

-- | Wrapper around 'SendContactRequest' request type for 'sendContact' method.
instance RunTG SendContactRequest (Response Message) where
  runTG = runTG . sendContact

-- | Wrapper around 'GetUserProfilePhotosRequest' request type for 'getUserProfilePhotos' method.
instance RunTG GetUserProfilePhotosRequest (Response UserProfilePhotos) where
  runTG = runTG . getUserProfilePhotos
