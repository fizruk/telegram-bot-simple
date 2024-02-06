# telegram-bot-api

## 7.0 -- 2024-02-06

- Support GHC 9.6 (see [#163](https://github.com/fizruk/telegram-bot-simple/pull/163)).

### Bot API 6.8 support

See [#165](https://github.com/fizruk/telegram-bot-simple/pull/165).

- Add new methods:
  - `unpinAllGeneralForumTopicMessages`.
- Add new types:
  - `Story`.
- Modify existing types:
  - `Message`, `Chat`, `PollAnswer`.

### Bot API 6.9 support

See [#168](https://github.com/fizruk/telegram-bot-simple/pull/168).

- Modify methods:
  - `promoteChatMember`.
- Modify types:
  - `ChatMember`.
  - `ChatAdministratorRights`
  - `WriteAccessAllowed`.

### Bot API 7.0 support

See [#169](https://github.com/fizruk/telegram-bot-simple/pull/169).

- Reactions:
  - New types: `ReactionType`, `ReactionCount`, `MessageReactionUpdated`, `MessageReactionCountUpdated`.
  - Modified types: `Chat`.
  - New method: `setMessageReaction`.
- Replies 2.0:
  - New types: `ExternalReplyInfo`, `MessageOrigin`, `TextQuote`, `ReplyParameters`.
  - Modified types: `Message`.
  - Modified methods: `copyMessage`, `sendAnimation`, `sendAudio`, `sendContact`, `sendDice`, `sendDocument`, `sendGame`, `sendInvoice`, `sendLocation`, `sendMediaGroup`, `sendMessage`, `sendPhoto`, `sendPoll`, `sendSticler`, `sendVenue`, `sendVideo`, `sendVideoNote`, `sendVoice`.
- Link Preview Customization:
  - New type: `LinkPreviewOptions`.
  - Modified types: `Message`.
  - Modified methods: `editMessage`, `sendMessage`.
- Multiple Message Actions:
  - New methods: `copyMessages`, `deleteMessages` and `forwardMessages`.
- Request for multiple users:
  - Deteled type: `KeyboardButtonRequestUser`
  - New types: `KeyboardButtonRequestUsers`, `UsersShared`.
  - Modified types: `KeyboardButton`, `Message`.
- Chat Boost:
  - New types: `ChatBoost`, `ChatBoostRemoved`, `ChatBoostSource`, `ChatBoostUpdated`, `UserChatBoosts`.
  - New methods: `getUserChatBoosts`.
- Giveaway:
  - New types: `Giveaway`, `GiveawayCompleted`, `GiveawayCreated`, `GiveawayWinners`.
  - Modified type: `Message`.
- Other changes:
  - `Message` could become inaccessible, see `isInaccessibleMessage` helper.
  - `CallbackQuery` documentation improved.
  - `Chat` type has been updated with extra fields, also enabled preliminary support of `WebApp`.

## 6.7.1 -- 2023-06-26

- Fix Inline Mode (see [#157](https://github.com/fizruk/telegram-bot-simple/pull/157)).

## 6.7 -- 2023-04-29

### Bot API 6.6 support

See [#147](https://github.com/fizruk/telegram-bot-simple/pull/147) and [#152](https://github.com/fizruk/telegram-bot-simple/pull/152).

- Add new methods: 
    - `setMyDescription`, `getMyDecription`, `setMyShortDescription`, `getMyShortDescription`.
    - `setCustomEmojiStickerSetThumbnail`, `setStickerSetTitle`, `deleteStickerSet`, `setStickerEmojiList`, `setStickerKeywords`, `setStickerMaskPosition`.
- Modify following methods:
    - `sendSticker` (add `emoji`).
    - `createNewStickerSet`, `addStickerToSet` (`sticker` to `stickers`, introduced `InputSticker`).
    - `uploadStickerFile` (remove `png_sticker` and other formats, add `sticker`, `sticker_format` fields).
- Rename `thumb` to `thumbnail`:
    - Types: `Animation`, `Audio`, `Document`, `Sticker`, `Video`, `VideoNote`, `InputMediaAnimation`, `InputMediaAudio`, `InputMediaDocument`, `InputMediaVideo`, `StickerSet`.
    - Inlines: `InlineQueryResultPhoto`, `InlineQueryResultVideo`, `InlineQueryResultGif`, `InlineQueryResultMpeg4Gif`.
    - Methods: `setStickerThumb` (method renamed itself to `setStickerThumbnail`), `sendAnimation`, `sendAudio`, `sendDocument`, `sendVideo`, `sendVideoNote`, 
- Modify `Sticker` type: add `needs_repainting` field.

- **Breaking changes**: Given the amount of Bot API changes, common record fields were moved tonew  `InlineQueryResultGeneric` data type and all thumbnails were moved to new `InlineQueryResultGenericThumbnail` data type.

- **Migration guide**:

    1. Provide `InlineQueryResultGeneric` (see `defInlineQueryResultGeneric`).
    2. Provide `InlineQueryResultGenericThumbnail` (see `defInlineQueryResultGenericThumbnail`).
    3. Specify your own `InlineQueryResult` (see helpers for each data constructor).

### Bot API 6.7 support

See [#155](https://github.com/fizruk/telegram-bot-simple/pull/155).

- Modify `answerInlineQuery` method.
- Modify `WriteAccessAllowed` data type.
- Add missing method `switchInlineQueryChosenChat`.
- Modify `ChatMemberUpdated` data type.
- Add new methods: `setMyName`, `getMyName`.


## 6.5.1 -- 2023-03-21

- Add new methods `getMyDescription`, `getMyShortDescription`, `setMyDescription`, `setMyShortDescription` (see [#141](https://github.com/fizruk/telegram-bot-simple/pull/141)).
- Re-export Forum, Games, Payments and Stickers in `Telegram.Bot.API.Methods` (see [#143](https://github.com/fizruk/telegram-bot-simple/issues/143)).

## 6.5 (Telegram Bot API 6.5)

- Package was originated at 6.5.
