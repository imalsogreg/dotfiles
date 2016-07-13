let

  nixpkgs = (import <nixpkgs> {});
  # lifted from http://github.com/reflex-frp/reflex-platform
  cabal2nixResult = src: nixpkgs.runCommand "cabal2nixResult" {
    buildCommand = ''
      cabal2nix file://"${src}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      cabal2nix
    ];
    # Support unicode characters in cabal files
    ${if !nixpkgs.stdenv.isDarwin then "LOCALE_ARCHIVE" else null} = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
    ${if !nixpkgs.stdenv.isDarwin then "LC_ALL" else null} = "en_US.UTF-8";
  };


in {

  packageOverrides = super: let self = super.pkgs; in {

    haskellPackages = super.haskellPackages.override {
      overrides = self: super: {
         snap-loader-dynamic = cabal2nixResult /home/greghale/Programming/tagging/deps/servant-snap/deps/snap-loader-dynamic {};
      };

    };

    myHaskellEnv = self.haskell.packages.ghc7102.ghcWithPackages
                     (haskellPackages: with haskellPackages; [
                        arrows async cgi criterion
                        cabal-install haskintex snap http-api-data
                      ]);
    all = with self; buildEnv {
      name = "all";
      paths = [
      ];
    };

  myTexLive = with super.pkgs; texLiveAggregationFun {
    paths = [ texLive texLiveExtra texLiveBeamer ];
  };

  };
  allowBroken = true;
  allowUnfree = true;
}
