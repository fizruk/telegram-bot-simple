0.9
---

- Make package complaint with Telegram Bot API 6.5 (breaking changes included) (see [#135](https://github.com/fizruk/telegram-bot-simple/pull/135)):

    - `KeyboardButton` extended with `request_user` and `request_chat` fields;
    - `KeyboardButtonRequestUser` and `KeyboardButtonRequestChat` types added;
    - `Message` extended with `user_shared` and `chat_shared` fields;
    - `ChatMember` and `ChatPermissions`: `can_send_media_messages` replaced with following fields:
        - `can_send_audios`;
        - `can_send_documents`;
        - `can_send_photos`;
        - `can_send_videos`;
        - `can_send_video_notes`;
        - `can_send_voice_notes`;
    - `use_independent_chat_permissions` field added to `setChatPermissions` and `restrictChatMember` methods;
    - `ChatJoinRequest` extended with `user_chat_id` field.
    - `Update` type fixed and aligned with the spec.

0.8
---

- Make package complaint with Telegram Bot API 6.4 (breaking changes included) (see [#134](https://github.com/fizruk/telegram-bot-simple/pull/134)):

    - `ReplyKeyboardMarkup` extended with `is_persistent`;
    - `Message` extended with `has_media_spoiler`;
    - `Chat` extended with `has_hidden_members` and `has_aggressive_anti_spam_enabled`;
    - `editForumTopic` method: fields `name` and `icon_custom_emoji_id` become optional;
    - `has_spoiler` field added to:
       - data constructors: `InputMediaPhoto`, `InputMediaVideo` and `InputMediaAnimation`;
       - method: `sendPhoto`, `sendVideo` and `sendAnimation`;
    - Following message events added `ForumTopicEdited`, `GeneralForumTopicHidden` and `GeneralForumTopicUnhidden`;
    - General topic forum management, i.e. added following functions: `editGeneralForumTopic`, `closeGeneralForumTopic`, `reopenGeneralForumTopic`, `hideGeneralForumTopic` and `unhideGeneralForumTopic`;

0.7
---

- Make package complaint with Telegram Bot API 6.3 (breaking changes included) (see [#133](https://github.com/fizruk/telegram-bot-simple/pull/133)):

    - `Chat` extended with `is_forum`, `active_usernames` and `emoji_status_custom_emoji_id`;
    - `Message` extended with `forum_topic_created`, `forum_topic_closed` and `forum_topic_reopened`;
    - Following message events added `ForumTopicCreated`, `ForumTopicClosed` and `ForumTopicReopened`;
    - `can_manage_topics` field added to `ChatAdministratorRights`, `ChatPermissions` and `ChatMember` data types;
    - `PromoteChatMemberRequest` extended with `can_manage_topics` too;
    - `message_thread_id` field added to following methods requests: `sendMessage`, `sendPhoto`, `sendVideo`, `sendAnimation`, `sendAudio`, `sendDocument`, `sendSticker`, `sendVideoNote`, `sendVoice`, `sendLocation`, `sendVenue`, `sendContact`, `sendPoll`, `sendDice`, `sendInvoice`, `sendGame`, `sendMediaGroup`, `copyMessage` and `forwardMessage`;
    - Forum management, i.e. added following functions: `createForumTopic`, `editForumTopic`, `closeForumTopic`, `reopenForumTopic`, `deleteForumTopic`, `unpinAllForumTopicMessages` and `getForumTopicIconStickers`;

0.6.2
---

- Fix decoding for `VideoChatStarted` (see [#132](https://github.com/fizruk/telegram-bot-simple/pull/132));

0.6.1
---

- Fix response parsing in `UpdatingMessages` (see [#131](https://github.com/fizruk/telegram-bot-simple/pull/131));
- Move from short polling to long polling (see [#129](https://github.com/fizruk/telegram-bot-simple/pull/129));

0.6
---

- Fix newlines unexpected removing in `UpdateParser` module (see [#124](https://github.com/fizruk/telegram-bot-simple/pull/124));
- Add WebHooks support (see [#127](https://github.com/fizruk/telegram-bot-simple/pull/127));
- Fix `parse_mode` encoding in `sendPhoto`, `sendDocument`, `sendVideo` and `sendAnimation` methods (see [#123](https://github.com/fizruk/telegram-bot-simple/pull/123) and [5ee5289](https://github.com/fizruk/telegram-bot-simple/commit/5ee5289f711381a1f1e0daf540bc6c041e3cb275));
- Make package complaint with Telegram Bot API 6.2 (breaking changes included) (see [fab1aee](https://github.com/fizruk/telegram-bot-simple/commit/fab1aee308afc59663013a796ebff7a3f99c8201)):

    - Added `createInvoiceLink` method;
    - Added `getCustomEmojiStickers` method;
    - `User` extended with `is_premium` and `added_to_attachment_menu`;
    - `Chat` extended with `has_restricted_voice_and_video_messages`, `join_to_send_messages`, `join_by_request`;
    - `MessageEntityType` extended with `custom_emoji`;
    `file_size` field for `Animation`, `Audio`, `Document`, `Video`, `VideoNote` `Voice`, `File` changed from `Int` to `Integer`;
    - `SticketSet` modified: `contains_masks` is optional now and becoming a subject of further deprecation in a future, `type` added;
    - Added `StickerSetType`: could be `regular`, `mask` and `custom_emoji`.

0.5.2
---

- Helpers from `UpdateParser` module fixed, `commandWithBotName` added (see [#121](https://github.com/fizruk/telegram-bot-simple/pull/121));

0.5.1
---

- Fix inconsistency around `ParseMode` (see [#114](https://github.com/fizruk/telegram-bot-simple/pull/114));
- Fix `copyMessage` method (see [#116](https://github.com/fizruk/telegram-bot-simple/pull/116));
- Add CI via GitHub Actions (see [#119](https://github.com/fizruk/telegram-bot-simple/pull/119));
- Fix GHC 9.0 compilation regression (see [#120](https://github.com/fizruk/telegram-bot-simple/pull/120));

0.5
---

- Add `MarkdownV2` to `ParseMode` haddock suggestion (see [#110](https://github.com/fizruk/telegram-bot-simple/pull/110));
- Make package complaint with Telegram Bot API 6.0 (breaking changes included) (see [#112](https://github.com/fizruk/telegram-bot-simple/pull/112)):

    - `WebAppInfo` data type added. `KeyboardButton`, `InlineKeyboardButton` extended with `web_app` record.
    - `SentWebAppMessage` data type added. `answerWebAppQuery` method added.
    - `MenuButton` data type added. `setChatMenuButton`, `getChatMenuButton` methods added.
    - `ChatAdministratorRights` data type added. `setMyDefaultAdministratorRights`, `getMyDefaultAdministratorRights` methods added.
    - :warning: `ChatMember` records changed: `can_manage_voice_chats` renamed to `can_manage_video_chat`.
    - :warning: `PromoteChatMemberRequest` records changed: `can_manage_voice_chats` renamed to `can_manage_video_chats`.
    - :warning: `Message` records changed: `voice_chat_scheduled`, `voice_chat_started`, `voice_chat_ended`, and `voice_chat_participants_invited` renamed to `video_chat_scheduled`, `video_chat_started`, `video_chat_ended`, and `video_chat_participants_invited` correspondingly.

0.4.5
---

- Fix `UpdateId` overflow exception (see [#108](https://github.com/fizruk/telegram-bot-simple/pull/108));
- Hide examples under flag `examples` (see [#109](https://github.com/fizruk/telegram-bot-simple/pull/109));

0.4.4
---

- Fix build on Windows (see [#107](https://github.com/fizruk/telegram-bot-simple/pull/107));

0.4.3
---

- Add support `spoiler` message type and `protect_content` field for various methods (see [#103](https://github.com/fizruk/telegram-bot-simple/pull/103));

0.4.2
---

- Fix GHC 9.0 compilation (see [#102](https://github.com/fizruk/telegram-bot-simple/pull/102) );

0.4.1
---

- Game improvements (see [#99](https://github.com/fizruk/telegram-bot-simple/pull/99) );
- Aeson >= 2.0 support (see [#100](https://github.com/fizruk/telegram-bot-simple/pull/100) );

0.4
---

- Add Payments methods (see [#93](https://github.com/fizruk/telegram-bot-simple/pull/93));
- Add Passport methods (see [#90](https://github.com/fizruk/telegram-bot-simple/pull/90));
- Resolve discrepancies with Bot API 5.5 (see [#87](https://github.com/fizruk/telegram-bot-simple/pull/87), [#88](https://github.com/fizruk/telegram-bot-simple/pull/88))
- Make `startPolling` polymorphic (see [#86](https://github.com/fizruk/telegram-bot-simple/pull/86));
- Add Updating messages methods (see [#85](https://github.com/fizruk/telegram-bot-simple/pull/85)) ;
- Add missing methods (see [#83](https://github.com/fizruk/telegram-bot-simple/pull/83), [#84](https://github.com/fizruk/telegram-bot-simple/pull/84));
- Add `GameBot` (see [#82](https://github.com/fizruk/telegram-bot-simple/pull/82), [#95](https://github.com/fizruk/telegram-bot-simple/pull/95));
- Allow return different types in `BotM` (see [#79](https://github.com/fizruk/telegram-bot-simple/pull/79), [#98](https://github.com/fizruk/telegram-bot-simple/pull/98));
- Fix `UserId` integer overflow (see [#78](https://github.com/fizruk/telegram-bot-simple/pull/78));
- Upgrade `EchoBot` example with sticker replies (see [#77](https://github.com/fizruk/telegram-bot-simple/pull/77));
- Refactor file uploads (see [#76](https://github.com/fizruk/telegram-bot-simple/pull/76));
- Add Stickers methods (see [#72](https://github.com/fizruk/telegram-bot-simple/pull/72), [#73](https://github.com/fizruk/telegram-bot-simple/pull/73), [#74](https://github.com/fizruk/telegram-bot-simple/pull/74) and [#75](https://github.com/fizruk/telegram-bot-simple/pull/75));
- Refactor `FileInfo` (see [#71](https://github.com/fizruk/telegram-bot-simple/pull/71));
- Add Game methods (see [#70](https://github.com/fizruk/telegram-bot-simple/pull/70));
- Fix `MessageId` integer overflow (see [#69](https://github.com/fizruk/telegram-bot-simple/pull/69));
- Add missing types (see [#66](https://github.com/fizruk/telegram-bot-simple/pull/66), [#81](https://github.com/fizruk/telegram-bot-simple/pull/81));

0.3.8
---

- Fix notification disable mechanism in `sendDocument` function (see [#54]( https://github.com/fizruk/telegram-bot-simple/pull/54 ));
- Implement `getFile` and `sendPhoto` API methods (see [#49]( https://github.com/fizruk/telegram-bot-simple/pull/49 ));

0.3.7
---

- Add inline mode support (see [#45](https://github.com/fizruk/telegram-bot-simple/pull/45));

0.3.6
---

- Remove extra spaces, restore Tested-with (see [#48](https://github.com/fizruk/telegram-bot-simple/pull/48));
- Add support of ghc 9 (see [#47]( https://github.com/fizruk/telegram-bot-simple/pull/47 ));

0.3.5
---

- Restore backward compatibility with older GHCs (see [#40](https://github.com/fizruk/telegram-bot-simple/pull/40));

0.3.4
---

- Fix cron job, restrict dependency on cron (see [#38](https://github.com/fizruk/telegram-bot-simple/pull/38), [#39](https://github.com/fizruk/telegram-bot-simple/pull/39));

0.3.3
---

- Derive Hashable for MessageId (see [#36](https://github.com/fizruk/telegram-bot-simple/pull/36));

0.3.2
---

- Implement forwardMessage API method (see [#35](https://github.com/fizruk/telegram-bot-simple/pull/35));
- Add MarkdownV2 parse mode (see [#33](https://github.com/fizruk/telegram-bot-simple/pull/33));

0.3.1
---

- Implement sendDocument function support. (see [#31](https://github.com/fizruk/telegram-bot-simple/pull/31));
- Add Travis CI (see [#32](https://github.com/fizruk/telegram-bot-simple/pull/32));
- Add MonadFail instance for UpdateParser (see [#27](https://github.com/fizruk/telegram-bot-simple/pull/27));

0.3.0
---

- Add `underline` and `strikethrough` `MessageEntityTypes` (see [#25](https://github.com/fizruk/telegram-bot-simple/pull/25));
- Fix for Stack 15 (see [#24](https://github.com/fizruk/telegram-bot-simple/pull/24));
- Fix installation after breaking change in `servant-0.16` (see [#21](https://github.com/fizruk/telegram-bot-simple/pull/21));
- Add `phonenumber` type (see [#23](https://github.com/fizruk/telegram-bot-simple/pull/23));
- Add `cashtag` message entity type (close #19) (see [#20](https://github.com/fizruk/telegram-bot-simple/pull/20));
- Feat/kick chat member (see [#17](https://github.com/fizruk/telegram-bot-simple/pull/17));
- Feat/delete message method (see [#16](https://github.com/fizruk/telegram-bot-simple/pull/16));
- Fix `startBotAsync`, add `processActionsIndefinitely` (see [#12](https://github.com/fizruk/telegram-bot-simple/pull/12));
- Add some badges to README (Hackage/Stackage/Travis) (see [11f13f3](https://github.com/fizruk/telegram-bot-simple/commit/11f13f3));
- Remove temporary `files/scripts` from repo (see [6bc9f48](https://github.com/fizruk/telegram-bot-simple/commit/6bc9f48));
- Add info about LambdaConf 2018 workshop and contributing (see [1ba4d95](https://github.com/fizruk/telegram-bot-simple/commit/1ba4d95));
- Add `Data.Monoid` import to fix builds on GHC 8.2 (see [c798001](https://github.com/fizruk/telegram-bot-simple/commit/c798001));

0.2.0
---

* Major changes:
  - Add bot jobs support (see [`9e0424e`](https://github.com/fizruk/telegram-bot-simple/commit/9e0424e));
  - Add `Telegram.Bot.Simple.Debug` (see [`7db84c5`](https://github.com/fizruk/telegram-bot-simple/commit/7db84c5),
    [`49679d4`](https://github.com/fizruk/telegram-bot-simple/commit/49679d4),
    [`5ba949b`](https://github.com/fizruk/telegram-bot-simple/commit/5ba949b));
  - Introduce `BotEnv` with model state and action queue (see [`98c869a`](https://github.com/fizruk/telegram-bot-simple/commit/98c869a));
  - Add support for message editing (see [`b7c83a4`](https://github.com/fizruk/telegram-bot-simple/commit/b7c83a4));
  - Introduce `replyOrEdit` helper (see [`ecc21cd`](https://github.com/fizruk/telegram-bot-simple/commit/ecc21cd));
  - Add useLatestUpdateInJobs helper to enable reply in jobs (see [`385f9e6`](https://github.com/fizruk/telegram-bot-simple/commit/385f9e6),
    [`8a12ceb`](https://github.com/fizruk/telegram-bot-simple/commit/8a12ceb),
    [`448bcd2`](https://github.com/fizruk/telegram-bot-simple/commit/448bcd2));

* Minor changes:
  - Add `getEnvToken` helper (see [`ce7d1f7`](https://github.com/fizruk/telegram-bot-simple/commit/ce7d1f7));
  - Add `IsString` instance for `Telegram.Token` (see [`f105bb9`](https://github.com/fizruk/telegram-bot-simple/commit/f105bb9));
  - Print Servant errors when `getUpdates` fails (see [`bc7c5bb`](https://github.com/fizruk/telegram-bot-simple/commit/bc7c5bb));
  - Split `Telegram.Bot.Simple` into several submodules (see [`8ed2783`](https://github.com/fizruk/telegram-bot-simple/commit/8ed2783));
  - Add `withEffect` helper in `Telegram.Bot.Simple.Eff` (see [`aebba52`](https://github.com/fizruk/telegram-bot-simple/commit/aebba52));
  - More Haddock documentation;

* Fixes:
  - Resolve #7 (see [#8](https://github.com/fizruk/telegram-bot-simple/pull/8));
  - Fix undefined in startBotAsync and add more documentation (see [`7879066`](https://github.com/fizruk/telegram-bot-simple/commit/7879066));
  - Fix inline buttons issue (see [#10](https://github.com/fizruk/telegram-bot-simple/pull/10));
