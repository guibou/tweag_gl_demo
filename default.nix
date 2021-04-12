{ pkgs ? import ./nixpkgs.nix { config = {}; overlays = []; } }: rec {
  inherit pkgs;

  # Explicit list of used files. Else there is always too much and
  # cache is invalidated.
  sources = pkgs.lib.sourceByRegex ./. [
    "gl-sample.cabal$"
    ".*.hs$"
    ".*.md$"
    "src"
    "app"
    "tests"
    "LICENSE"
  ];

  shell = app.env.overrideAttrs (old: {
    # The Haskell environment does not come with cabal-install
    nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.cabal-install ];
  });

  # Shell with Haskell language server
  shell_hls = shell.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs
      ++ [ pkgs.haskellPackages.haskell-language-server ];
  });

  app = (pkgs.haskell.lib.buildFromSdist ((pkgs.haskellPackages.override {
    overrides = self: super: {
      wavefront = pkgs.haskell.lib.unmarkBroken (pkgs.haskell.lib.doJailbreak super.wavefront);
    };
  }).callCabal2nix "gl-sample" sources { })).overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs;
    passthru = oldAttrs.passthru // { inherit shell shell_hls; };
  });

  formatting-fix = pkgs.mkShell {
    nativeBuildInputs =
      [ pkgs.haskellPackages.ormolu pkgs.haskellPackages.brittany pkgs.git ];
    shellHook = ''
      # Format with britanny. It does have a policy for line break
      brittany --write-mode inplace **/*.hs
      # Format with ormolu, it works will
      ormolu --mode inplace **/*.hs
      exit 0
    '';
  };
}
