{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Methods
  ( AnswerCallbackQuery
  , ApproveChatJoinRequest
  , BanChatMember
  , BanChatSenderChat
  , CopyMessage
  , CreateChatInviteLink
  , DeclineChatJoinRequest
  , DeleteChatPhoto
  , DeleteChatStickerSet
  , DeleteMessage
  , DeleteMyCommands
  , EditChatInviteLink
  , EditMessageLiveLocation
  , ExportChatInviteLink
  , ForwardMessage
  , GetChat
  , GetChatAdministrators
  , GetChatMember
  , GetChatMemberCount
  , GetChatMenuButton
  , GetFile
  , GetMe
  , GetMyCommands
  , GetMyDefaultAdministratorRights
  , GetUserProfilePhotos
  , LeaveChat
  , PinChatMessage
  , PromoteChatMember
  , RestrictChatMember
  , RevokeChatInviteLink
  , SendAnimationContent
  , SendAnimationLink
  , SendAudioContent
  , SendAudioLink
  , SendChatAction
  , SendContact
  , SendDice
  , SendDocumentContent
  , SendDocumentLink
  , SendLocation
  , SendMediaGroup
  , SendMessage
  , SendPhotoContent
  , SendPhotoLink
  , SendPoll
  , SendVenue
  , SendVideoContent
  , SendVideoLink
  , SendVideoNoteContent
  , SendVideoNoteLink
  , SendVoiceContent
  , SendVoiceLink
  , SetChatAdministratorCustomTitle
  , SetChatDescription
  , SetChatMenuButton
  , SetChatPermissions
  , SetChatPhoto
  , SetChatStickerSet
  , SetChatTitle
  , SetMyCommands
  , SetMyDefaultAdministratorRights
  , StopMessageLiveLocation
  , UnbanChatMember
  , UnbanChatSenderChat
  , UnpinAllChatMessages
  , UnpinChatMessage

  , ParseMode (..)
  , SomeReplyMarkup (..)

  , AnswerCallbackQueryRequest (..)
  , BanChatMemberRequest (..)
  , CopyMessageRequest (..)
  , CreateChatInviteLinkRequest (..)
  , DeleteMyCommandsRequest (..)
  , EditChatInviteLinkRequest (..)
  , EditMessageLiveLocationRequest (..)
  , ForwardMessageRequest (..)
  , GetChatMenuButtonRequest (..)
  , GetMyCommandsRequest (..)
  , GetMyDefaultAdministratorRightsRequest (..)
  , GetUserProfilePhotosRequest (..)
  , PinChatMessageRequest (..)
  , PromoteChatMemberRequest (..)
  , RestrictChatMemberRequest (..)
  , SendAnimationRequest (..)
  , SendAudioRequest (..)
  , SendContactRequest (..)
  , SendDiceRequest (..)
  , SendDocumentRequest (..)
  , SendLocationRequest (..)
  , SendMediaGroupRequest (..)
  , SendMessageRequest (..)
  , SendPhotoRequest (..)
  , SendPollRequest (..)
  , SendVenueRequest (..)
  , SendVideoRequest (..)
  , SendVideoNoteRequest (..)
  , SendVoiceRequest (..)
  , SetChatAdministratorCustomTitleRequest (..)
  , SetChatMenuButtonRequest (..)
  , SetChatPermissionsRequest (..)
  , SetChatPhotoRequest (..)
  , SetMyCommandsRequest (..)
  , SetMyDefaultAdministratorRightsRequest (..)
  , StopMessageLiveLocationRequest (..)
  , UnbanChatMemberRequest (..)

  , answerCallbackQuery
  , approveChatJoinRequest
  , banChatMember
  , banChatSenderChat
  , copyMessage
  , createChatInviteLink
  , declineChatJoinRequest
  , deleteChatPhoto
  , deleteChatStickerSet
  , deleteMessage
  , deleteMyCommands
  , editChatInviteLink
  , editMessageLiveLocation
  , exportChatInviteLink
  , forwardMessage
  , getChat
  , getChatAdministrators
  , getChatMember
  , getChatMemberCount
  , getChatMenuButton
  , getFile
  , getMe
  , getMyCommands
  , getMyDefaultAdministratorRights
  , getUserProfilePhotos
  , leaveChat
  , pinChatMessage
  , promoteChatMember
  , restrictChatMember
  , revokeChatInviteLink
  , sendAnimation
  , sendAudio
  , sendChatAction
  , sendContact
  , sendDice
  , sendDocument
  , sendLocation
  , sendMediaGroup
  , sendMessage
  , sendPhoto
  , sendPoll
  , sendVenue
  , sendVideo
  , sendVideoNote
  , sendVoice
  , setChatAdministratorCustomTitle
  , setChatDescription
  , setChatMenuButton
  , setChatPermissions
  , setChatPhoto
  , setChatStickerSet
  , setChatTitle
  , setMyCommands
  , setMyDefaultAdministratorRights
  , stopMessageLiveLocation
  , unbanChatMember
  , unbanChatSenderChat
  , unpinAllChatMessages
  , unpinChatMessage
  ) where

import Telegram.Bot.API.Types.ParseMode
import Telegram.Bot.API.Types.SomeReplyMarkup

import Telegram.Bot.API.Methods.AnswerCallbackQuery
import Telegram.Bot.API.Methods.ApproveChatJoinRequest
import Telegram.Bot.API.Methods.BanChatMember
import Telegram.Bot.API.Methods.BanChatSenderChat
import Telegram.Bot.API.Methods.CopyMessage
import Telegram.Bot.API.Methods.CreateChatInviteLink
import Telegram.Bot.API.Methods.DeclineChatJoinRequest
import Telegram.Bot.API.Methods.DeleteChatPhoto
import Telegram.Bot.API.Methods.DeleteChatStickerSet
import Telegram.Bot.API.Methods.DeleteMessage
import Telegram.Bot.API.Methods.DeleteMyCommands
import Telegram.Bot.API.Methods.EditChatInviteLink
import Telegram.Bot.API.Methods.EditMessageLiveLocation
import Telegram.Bot.API.Methods.ExportChatInviteLink
import Telegram.Bot.API.Methods.ForwardMessage
import Telegram.Bot.API.Methods.GetChat
import Telegram.Bot.API.Methods.GetChatAdministrators
import Telegram.Bot.API.Methods.GetChatMember
import Telegram.Bot.API.Methods.GetChatMemberCount
import Telegram.Bot.API.Methods.GetChatMenuButton
import Telegram.Bot.API.Methods.GetFile
import Telegram.Bot.API.Methods.GetMe
import Telegram.Bot.API.Methods.GetMyCommands
import Telegram.Bot.API.Methods.GetMyDefaultAdministratorRights
import Telegram.Bot.API.Methods.GetUserProfilePhotos
import Telegram.Bot.API.Methods.LeaveChat
import Telegram.Bot.API.Methods.PinChatMessage
import Telegram.Bot.API.Methods.PromoteChatMember
import Telegram.Bot.API.Methods.RestrictChatMember
import Telegram.Bot.API.Methods.RevokeChatInviteLink
import Telegram.Bot.API.Methods.SendAnimation
import Telegram.Bot.API.Methods.SendAudio
import Telegram.Bot.API.Methods.SendChatAction
import Telegram.Bot.API.Methods.SendContact
import Telegram.Bot.API.Methods.SendDice
import Telegram.Bot.API.Methods.SendDocument
import Telegram.Bot.API.Methods.SendLocation
import Telegram.Bot.API.Methods.SendMediaGroup
import Telegram.Bot.API.Methods.SendMessage
import Telegram.Bot.API.Methods.SendPhoto
import Telegram.Bot.API.Methods.SendPoll
import Telegram.Bot.API.Methods.SendVenue
import Telegram.Bot.API.Methods.SendVideo
import Telegram.Bot.API.Methods.SendVideoNote
import Telegram.Bot.API.Methods.SendVoice
import Telegram.Bot.API.Methods.SetChatAdministratorCustomTitle
import Telegram.Bot.API.Methods.SetChatDescription
import Telegram.Bot.API.Methods.SetChatMenuButton
import Telegram.Bot.API.Methods.SetChatPermissions
import Telegram.Bot.API.Methods.SetChatPhoto
import Telegram.Bot.API.Methods.SetChatStickerSet
import Telegram.Bot.API.Methods.SetChatTitle
import Telegram.Bot.API.Methods.SetMyCommands
import Telegram.Bot.API.Methods.SetMyDefaultAdministratorRights
import Telegram.Bot.API.Methods.StopMessageLiveLocation
import Telegram.Bot.API.Methods.UnbanChatMember
import Telegram.Bot.API.Methods.UnbanChatSenderChat
import Telegram.Bot.API.Methods.UnpinAllChatMessages
import Telegram.Bot.API.Methods.UnpinChatMessage
