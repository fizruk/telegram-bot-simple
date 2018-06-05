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
