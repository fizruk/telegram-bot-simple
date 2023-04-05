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
  ( module Telegram.Bot.API.Types.ParseMode
  , module Telegram.Bot.API.Types.SomeReplyMarkup
  
  , module Telegram.Bot.API.Methods.AnswerCallbackQuery
  , module Telegram.Bot.API.Methods.ApproveChatJoinRequest
  , module Telegram.Bot.API.Methods.BanChatMember
  , module Telegram.Bot.API.Methods.BanChatSenderChat
  , module Telegram.Bot.API.Methods.CopyMessage
  , module Telegram.Bot.API.Methods.CreateChatInviteLink
  , module Telegram.Bot.API.Methods.DeclineChatJoinRequest
  , module Telegram.Bot.API.Methods.DeleteChatPhoto
  , module Telegram.Bot.API.Methods.DeleteChatStickerSet
  , module Telegram.Bot.API.Methods.DeleteMessage
  , module Telegram.Bot.API.Methods.DeleteMyCommands
  , module Telegram.Bot.API.Methods.EditChatInviteLink
  , module Telegram.Bot.API.Methods.EditMessageLiveLocation
  , module Telegram.Bot.API.Methods.ExportChatInviteLink
  , module Telegram.Bot.API.Methods.ForwardMessage
  , module Telegram.Bot.API.Methods.GetChat
  , module Telegram.Bot.API.Methods.GetChatAdministrators
  , module Telegram.Bot.API.Methods.GetChatMember
  , module Telegram.Bot.API.Methods.GetChatMemberCount
  , module Telegram.Bot.API.Methods.GetChatMenuButton
  , module Telegram.Bot.API.Methods.GetFile
  , module Telegram.Bot.API.Methods.GetMe
  , module Telegram.Bot.API.Methods.GetMyCommands
  , module Telegram.Bot.API.Methods.GetMyDefaultAdministratorRights
  , module Telegram.Bot.API.Methods.GetMyDescription
  , module Telegram.Bot.API.Methods.GetMyShortDescription
  , module Telegram.Bot.API.Methods.GetUserProfilePhotos
  , module Telegram.Bot.API.Methods.LeaveChat
  , module Telegram.Bot.API.Methods.PinChatMessage
  , module Telegram.Bot.API.Methods.PromoteChatMember
  , module Telegram.Bot.API.Methods.RestrictChatMember
  , module Telegram.Bot.API.Methods.RevokeChatInviteLink
  , module Telegram.Bot.API.Methods.SendAnimation
  , module Telegram.Bot.API.Methods.SendAudio
  , module Telegram.Bot.API.Methods.SendChatAction
  , module Telegram.Bot.API.Methods.SendContact
  , module Telegram.Bot.API.Methods.SendDice
  , module Telegram.Bot.API.Methods.SendDocument
  , module Telegram.Bot.API.Methods.SendLocation
  , module Telegram.Bot.API.Methods.SendMediaGroup
  , module Telegram.Bot.API.Methods.SendMessage
  , module Telegram.Bot.API.Methods.SendPhoto
  , module Telegram.Bot.API.Methods.SendPoll
  , module Telegram.Bot.API.Methods.SendVenue
  , module Telegram.Bot.API.Methods.SendVideo
  , module Telegram.Bot.API.Methods.SendVideoNote
  , module Telegram.Bot.API.Methods.SendVoice
  , module Telegram.Bot.API.Methods.SetChatAdministratorCustomTitle
  , module Telegram.Bot.API.Methods.SetChatDescription
  , module Telegram.Bot.API.Methods.SetChatMenuButton
  , module Telegram.Bot.API.Methods.SetChatPermissions
  , module Telegram.Bot.API.Methods.SetChatPhoto
  , module Telegram.Bot.API.Methods.SetChatStickerSet
  , module Telegram.Bot.API.Methods.SetChatTitle
  , module Telegram.Bot.API.Methods.SetMyCommands
  , module Telegram.Bot.API.Methods.SetCustomEmojiStickerSetThumbnail
  , module Telegram.Bot.API.Methods.SetMyDefaultAdministratorRights
  , module Telegram.Bot.API.Methods.SetMyDescription
  , module Telegram.Bot.API.Methods.SetMyShortDescription
  , module Telegram.Bot.API.Methods.StopMessageLiveLocation
  , module Telegram.Bot.API.Methods.UnbanChatMember
  , module Telegram.Bot.API.Methods.UnbanChatSenderChat
  , module Telegram.Bot.API.Methods.UnpinAllChatMessages
  , module Telegram.Bot.API.Methods.UnpinChatMessage
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
import Telegram.Bot.API.Methods.GetMyDescription
import Telegram.Bot.API.Methods.GetMyShortDescription
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
import Telegram.Bot.API.Methods.SetCustomEmojiStickerSetThumbnail
import Telegram.Bot.API.Methods.SetMyDefaultAdministratorRights
import Telegram.Bot.API.Methods.SetMyDescription
import Telegram.Bot.API.Methods.SetMyShortDescription
import Telegram.Bot.API.Methods.StopMessageLiveLocation
import Telegram.Bot.API.Methods.UnbanChatMember
import Telegram.Bot.API.Methods.UnbanChatSenderChat
import Telegram.Bot.API.Methods.UnpinAllChatMessages
import Telegram.Bot.API.Methods.UnpinChatMessage
