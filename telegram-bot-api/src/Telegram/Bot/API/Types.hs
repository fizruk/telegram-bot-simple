{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Telegram.Bot.API.Types
  ( RequiredQueryParam 
  , module Telegram.Bot.API.Types.Animation
  , module Telegram.Bot.API.Types.Audio
  , module Telegram.Bot.API.Types.BotCommand
  , module Telegram.Bot.API.Types.BotCommandScope
  , module Telegram.Bot.API.Types.BotDescription
  , module Telegram.Bot.API.Types.BotName
  , module Telegram.Bot.API.Types.BotShortDescription
  , module Telegram.Bot.API.Types.BusinessConnection
  , module Telegram.Bot.API.Types.BusinessMessagesDeleted
  , module Telegram.Bot.API.Types.CallbackGame
  , module Telegram.Bot.API.Types.CallbackQuery
  , module Telegram.Bot.API.Types.Chat
  , module Telegram.Bot.API.Types.ChatBoost
  , module Telegram.Bot.API.Types.ChatBoostRemoved
  , module Telegram.Bot.API.Types.ChatBoostSource
  , module Telegram.Bot.API.Types.ChatBoostUpdated
  , module Telegram.Bot.API.Types.ChatAdministratorRights
  , module Telegram.Bot.API.Types.ChatInviteLink
  , module Telegram.Bot.API.Types.ChatJoinRequest
  , module Telegram.Bot.API.Types.ChatLocation
  , module Telegram.Bot.API.Types.ChatMember
  , module Telegram.Bot.API.Types.ChatMemberUpdated
  , module Telegram.Bot.API.Types.ChatPermissions
  , module Telegram.Bot.API.Types.ChatPhoto
  , module Telegram.Bot.API.Types.ChatShared
  , module Telegram.Bot.API.Types.Common
  , module Telegram.Bot.API.Types.Contact
  , module Telegram.Bot.API.Types.CopyMessageId
  , module Telegram.Bot.API.Types.Dice
  , module Telegram.Bot.API.Types.Document
  , module Telegram.Bot.API.Types.EncryptedCredentials
  , module Telegram.Bot.API.Types.EncryptedPassportElement
  , module Telegram.Bot.API.Types.ExternalReplyInfo
  , module Telegram.Bot.API.Types.File
  , module Telegram.Bot.API.Types.ForceReply
  , module Telegram.Bot.API.Types.ForumTopic
  , module Telegram.Bot.API.Types.ForumTopicEdited
  , module Telegram.Bot.API.Types.ForumTopicClosed
  , module Telegram.Bot.API.Types.ForumTopicCreated
  , module Telegram.Bot.API.Types.ForumTopicReopened
  , module Telegram.Bot.API.Types.Game
  , module Telegram.Bot.API.Types.GameHighScore
  , module Telegram.Bot.API.Types.Giveaway
  , module Telegram.Bot.API.Types.GiveawayCompleted
  , module Telegram.Bot.API.Types.GiveawayCreated
  , module Telegram.Bot.API.Types.GiveawayWinners
  , module Telegram.Bot.API.Types.GeneralForumTopicHidden
  , module Telegram.Bot.API.Types.GeneralForumTopicUnhidden
  , module Telegram.Bot.API.Types.InlineKeyboardButton
  , module Telegram.Bot.API.Types.InlineKeyboardMarkup
  , module Telegram.Bot.API.Types.InputMedia
  , module Telegram.Bot.API.Types.Invoice
  , module Telegram.Bot.API.Types.KeyboardButton
  , module Telegram.Bot.API.Types.KeyboardButtonRequestChat
  , module Telegram.Bot.API.Types.KeyboardButtonRequestUsers
  , module Telegram.Bot.API.Types.LabeledPrice
  , module Telegram.Bot.API.Types.LinkPreviewOptions
  , module Telegram.Bot.API.Types.Location
  , module Telegram.Bot.API.Types.LoginUrl
  , module Telegram.Bot.API.Types.MaskPosition
  , module Telegram.Bot.API.Types.MenuButton
  , module Telegram.Bot.API.Types.Message
  , module Telegram.Bot.API.Types.MessageAutoDeleteTimerChanged
  , module Telegram.Bot.API.Types.MessageEntity
  , module Telegram.Bot.API.Types.MessageOrigin
  , module Telegram.Bot.API.Types.MessageReactionCountUpdated
  , module Telegram.Bot.API.Types.MessageReactionUpdated
  , module Telegram.Bot.API.Types.OrderInfo
  , module Telegram.Bot.API.Types.PassportData
  , module Telegram.Bot.API.Types.PassportElementError
  , module Telegram.Bot.API.Types.PassportFile
  , module Telegram.Bot.API.Types.PhotoSize
  , module Telegram.Bot.API.Types.Poll
  , module Telegram.Bot.API.Types.PollAnswer
  , module Telegram.Bot.API.Types.PollOption
  , module Telegram.Bot.API.Types.PollType
  , module Telegram.Bot.API.Types.PreCheckoutQuery
  , module Telegram.Bot.API.Types.ProximityAlertTriggered
  , module Telegram.Bot.API.Types.ReactionCount
  , module Telegram.Bot.API.Types.ReactionType
  , module Telegram.Bot.API.Types.ReplyKeyboardMarkup
  , module Telegram.Bot.API.Types.ReplyKeyboardRemove
  , module Telegram.Bot.API.Types.ReplyParameters
  , module Telegram.Bot.API.Types.ResponseParameters
  , module Telegram.Bot.API.Types.ShippingAddress
  , module Telegram.Bot.API.Types.ShippingOption
  , module Telegram.Bot.API.Types.ShippingQuery
  , module Telegram.Bot.API.Types.Sticker
  , module Telegram.Bot.API.Types.Story
  , module Telegram.Bot.API.Types.SuccessfulPayment
  , module Telegram.Bot.API.Types.User
  , module Telegram.Bot.API.Types.UserChatBoosts
  , module Telegram.Bot.API.Types.UserProfilePhotos
  , module Telegram.Bot.API.Types.UsersShared
  , module Telegram.Bot.API.Types.UserShared
  , module Telegram.Bot.API.Types.Venue
  , module Telegram.Bot.API.Types.Video
  , module Telegram.Bot.API.Types.VideoChatEnded
  , module Telegram.Bot.API.Types.VideoChatScheduled
  , module Telegram.Bot.API.Types.VideoChatStarted
  , module Telegram.Bot.API.Types.VideoNote
  , module Telegram.Bot.API.Types.Voice
  , module Telegram.Bot.API.Types.WebAppData
  , module Telegram.Bot.API.Types.WriteAccessAllowed
  ) where

import Servant.API


import Telegram.Bot.API.Types.Animation
import Telegram.Bot.API.Types.Audio
import Telegram.Bot.API.Types.BotCommand
import Telegram.Bot.API.Types.BotCommandScope
import Telegram.Bot.API.Types.BotDescription
import Telegram.Bot.API.Types.BotName
import Telegram.Bot.API.Types.BotShortDescription
import Telegram.Bot.API.Types.BusinessConnection
import Telegram.Bot.API.Types.BusinessMessagesDeleted
import Telegram.Bot.API.Types.CallbackGame
import Telegram.Bot.API.Types.CallbackQuery
import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.ChatBoost
import Telegram.Bot.API.Types.ChatBoostRemoved
import Telegram.Bot.API.Types.ChatBoostSource
import Telegram.Bot.API.Types.ChatBoostUpdated
import Telegram.Bot.API.Types.ChatAdministratorRights
import Telegram.Bot.API.Types.ChatInviteLink
import Telegram.Bot.API.Types.ChatJoinRequest
import Telegram.Bot.API.Types.ChatLocation
import Telegram.Bot.API.Types.ChatMember
import Telegram.Bot.API.Types.ChatMemberUpdated
import Telegram.Bot.API.Types.ChatPermissions
import Telegram.Bot.API.Types.ChatPhoto
import Telegram.Bot.API.Types.ChatShared
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.Contact
import Telegram.Bot.API.Types.CopyMessageId
import Telegram.Bot.API.Types.Dice
import Telegram.Bot.API.Types.Document
import Telegram.Bot.API.Types.EncryptedCredentials
import Telegram.Bot.API.Types.EncryptedPassportElement
import Telegram.Bot.API.Types.ExternalReplyInfo
import Telegram.Bot.API.Types.File
import Telegram.Bot.API.Types.ForceReply
import Telegram.Bot.API.Types.ForumTopic
import Telegram.Bot.API.Types.ForumTopicEdited
import Telegram.Bot.API.Types.ForumTopicClosed
import Telegram.Bot.API.Types.ForumTopicCreated
import Telegram.Bot.API.Types.ForumTopicReopened
import Telegram.Bot.API.Types.Game
import Telegram.Bot.API.Types.GameHighScore
import Telegram.Bot.API.Types.Giveaway
import Telegram.Bot.API.Types.GiveawayCompleted
import Telegram.Bot.API.Types.GiveawayCreated
import Telegram.Bot.API.Types.GiveawayWinners
import Telegram.Bot.API.Types.GeneralForumTopicHidden
import Telegram.Bot.API.Types.GeneralForumTopicUnhidden
import Telegram.Bot.API.Types.InlineKeyboardButton
import Telegram.Bot.API.Types.InlineKeyboardMarkup
import Telegram.Bot.API.Types.InputMedia
import Telegram.Bot.API.Types.Invoice
import Telegram.Bot.API.Types.KeyboardButton
import Telegram.Bot.API.Types.KeyboardButtonRequestChat
import Telegram.Bot.API.Types.KeyboardButtonRequestUsers
import Telegram.Bot.API.Types.LabeledPrice
import Telegram.Bot.API.Types.LinkPreviewOptions
import Telegram.Bot.API.Types.Location
import Telegram.Bot.API.Types.LoginUrl
import Telegram.Bot.API.Types.MaskPosition
import Telegram.Bot.API.Types.MenuButton
import Telegram.Bot.API.Types.Message
import Telegram.Bot.API.Types.MessageAutoDeleteTimerChanged
import Telegram.Bot.API.Types.MessageEntity
import Telegram.Bot.API.Types.MessageOrigin
import Telegram.Bot.API.Types.MessageReactionCountUpdated
import Telegram.Bot.API.Types.MessageReactionUpdated
import Telegram.Bot.API.Types.OrderInfo
import Telegram.Bot.API.Types.PassportData
import Telegram.Bot.API.Types.PassportElementError
import Telegram.Bot.API.Types.PassportFile
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Types.Poll
import Telegram.Bot.API.Types.PollAnswer
import Telegram.Bot.API.Types.PollOption
import Telegram.Bot.API.Types.PollType
import Telegram.Bot.API.Types.PreCheckoutQuery
import Telegram.Bot.API.Types.ProximityAlertTriggered
import Telegram.Bot.API.Types.ReactionCount
import Telegram.Bot.API.Types.ReactionType
import Telegram.Bot.API.Types.ReplyKeyboardMarkup
import Telegram.Bot.API.Types.ReplyKeyboardRemove
import Telegram.Bot.API.Types.ReplyParameters
import Telegram.Bot.API.Types.ResponseParameters
import Telegram.Bot.API.Types.ShippingAddress
import Telegram.Bot.API.Types.ShippingOption
import Telegram.Bot.API.Types.ShippingQuery
import Telegram.Bot.API.Types.Sticker
import Telegram.Bot.API.Types.Story
import Telegram.Bot.API.Types.SuccessfulPayment
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Types.UserChatBoosts
import Telegram.Bot.API.Types.UserProfilePhotos
import Telegram.Bot.API.Types.UsersShared
import Telegram.Bot.API.Types.UserShared
import Telegram.Bot.API.Types.Venue
import Telegram.Bot.API.Types.Video
import Telegram.Bot.API.Types.VideoChatEnded
import Telegram.Bot.API.Types.VideoChatScheduled
import Telegram.Bot.API.Types.VideoChatStarted
import Telegram.Bot.API.Types.VideoNote
import Telegram.Bot.API.Types.Voice
import Telegram.Bot.API.Types.WebAppData
import Telegram.Bot.API.Types.WriteAccessAllowed

type RequiredQueryParam = QueryParam' '[Required , Strict]

-- * Available types




-- * Stickers

-- | The following methods and objects allow your bot to handle stickers and sticker sets.


-- * Payments

-- * Telegram Passport

-- | Telegram Passport is a unified authorization method for services that require personal identification. Users can upload their documents once, then instantly share their data with services that require real-world ID (finance, ICOs, etc.). Please see the manual for details.

-- * Games

-- | Your bot can offer users HTML5 games to play solo or to compete against each other in groups and one-on-one chats. Create games via @BotFather using the /newgame command. Please note that this kind of power requires responsibility: you will need to accept the terms for each game that your bots will be offering.
-- 
-- Games are a new type of content on Telegram, represented by the Game and InlineQueryResultGame objects.
-- Once you've created a game via BotFather, you can send games to chats as regular messages using the sendGame method, or use inline mode with InlineQueryResultGame.
-- If you send the game message without any buttons, it will automatically have a 'Play GameName' button. When this button is pressed, your bot gets a CallbackQuery with the game_short_name of the requested game. You provide the correct URL for this particular user and the app opens the game in the in-app browser.
-- You can manually add multiple buttons to your game message. Please note that the first button in the first row must always launch the game, using the field callback_game in InlineKeyboardButton. You can add extra buttons according to taste: e.g., for a description of the rules, or to open the game's official community.
-- To make your game more attractive, you can upload a GIF animation that demostrates the game to the users via BotFather (see Lumberjack for example).
-- A game message will also display high scores for the current chat. Use setGameScore to post high scores to the chat with the game, add the edit_message parameter to automatically update the message with the current scoreboard.
-- Use getGameHighScores to get data for in-game high score tables.
-- You can also add an extra sharing button for users to share their best score to different chats.
-- For examples of what can be done using this new stuff, check the @gamebot and @gamee bots.

