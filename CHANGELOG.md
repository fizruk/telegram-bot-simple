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
