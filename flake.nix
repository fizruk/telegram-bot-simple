{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils/cfacdce06f30d2b68473a46042957675eebb3401";
    nixpkgs.url = "github:NixOS/nixpkgs/1fb781f4a148c19e9da1d35a4cbe15d0158afc4e";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};

      telegram-bot-api = "telegram-bot-api";
      telegram-bot-simple = "telegram-bot-simple";

      systemDepends = [ pkgs.zlib ];
      override = {
        overrides = self: super:
          let inherit (pkgs.haskell.lib) overrideCabal; in
          {
            "${telegram-bot-api}" = overrideCabal
              (super.callCabal2nix telegram-bot-api ./${telegram-bot-api} { })
              (x: { librarySystemDepends = systemDepends ++ (x.librarySystemDepends or [ ]); });
            "${telegram-bot-simple}" = overrideCabal
              (self.callCabal2nix telegram-bot-simple ./${telegram-bot-simple} {
                "${telegram-bot-api}" = self."${telegram-bot-api}";
              })
              (x: { librarySystemDepends = systemDepends ++ (x.librarySystemDepends or [ ]); });
          };
      };

      ghcVersion = "ghc927";
      hpkgs = pkgs.haskell.packages.${ghcVersion};

      getHaskellPackagesDeps = packages_:
        with pkgs.lib.lists; pkgs.lib.lists.unique (
          subtractLists packages_ (
            concatLists (map
              (package:
                __filter pkgs.lib.attrsets.isDerivation (concatLists (
                  __attrValues package.getCabalDeps
                )))
              packages_)
          )
        );

      ghcForPackages = hpkgs_:  override_: packageNames_:
        (hpkgs_.override override_).ghcWithPackages (ps:
          getHaskellPackagesDeps (map (x: ps.${x}) packageNames_)
        );

      ghc = ghcForPackages hpkgs override [ telegram-bot-api telegram-bot-simple ];

      tools = [
        hpkgs.cabal-install
        # ghc should go before haskell-language-server - https://github.com/NixOS/nixpkgs/issues/225895
        ghc
        hpkgs.haskell-language-server

        pkgs.dhall-lsp-server
      ];

      devShells.default = pkgs.mkShell {
        buildInputs = tools;
        shellHook = "export LANG=C.utf8";
      };
    in
    {
      inherit devShells;
    });
}
