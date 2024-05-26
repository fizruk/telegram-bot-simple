# telegram-bot-simple

[![Hackage](https://img.shields.io/hackage/v/telegram-bot-simple.svg)](http://hackage.haskell.org/package/telegram-bot-simple)
[![Stackage LTS](http://stackage.org/package/telegram-bot-simple/badge/lts)](http://stackage.org/lts/package/telegram-bot-simple)
[![Stackage Nightly](http://stackage.org/package/telegram-bot-simple/badge/nightly)](http://stackage.org/nightly/package/telegram-bot-simple)

Easy to use library for building Telegram bots in Haskell.

## LambdaConf 2018 workshop

This library was featured in a [LambdaConf 2018 workshop](https://lambdaconf2018.dryfta.com/en/program-schedule/program/32/building-a-telegram-bot-in-haskell).
The supplementary materials for the workshop is available at https://github.com/fizruk/lambdaconf-2018-workshop.

## Examples

See bot examples here: https://github.com/fizruk/telegram-bot-simple/tree/master/telegram-bot-simple/examples

Use `cabal build all -fexamples` to build it.
If you are building with stack then use `stack build --flag telegram-bot-simple:examples`.

## Contributing

Contributions are welcome!
Feel free to ping me on GitHub, file an issue or submit a PR :)

For local development you can use following command:

```
cabal repl -fexamples <target> -O0
```

where `<target>` could be `telegram-bot-simple` or `telegram-bot-api`


### Nix

You can use a [Nix flake](https://wiki.nixos.org/wiki/Flakes) from this repo to get several development tools.

1. [Enable flakes](https://wiki.nixos.org/wiki/Flakes#Enable_flakes).

2. Run `nix develop`. This command will make available `cabal`, `ghc`, and `haskell-language-server`.

3. Run `cabal run example-echo-bot` to start a bot.

## Compatibility

| telegram-bot-simple  | telegram-bot-api |
| ------------- | ------------- |
| 0.10  | 6.5  |
| 0.11.1  | 6.5.1  |
| 0.12 | 6.7.1 |
| 0.13 | 7.0 | 
| 0.14 | 7.3 | 
| 0.14.1 | 7.3.1 | 

_Nick_
