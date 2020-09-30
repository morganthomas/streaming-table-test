{ chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
, compiler ? "ghc864"
, withHoogle ? false
, doHoogle ? false
, doHaddock ? false
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
, strictDeps ? false
, isJS ? false
, asShell ? false
, localShpadoinkle ? false
}:
let


  # It's a shpadoinkle day
  shpadoinkle = if localShpadoinkle then ../Shpadoinkle else builtins.fetchGit {
    url    = https://gitlab.com/morganthomas/Shpadoinkle.git;
    rev    = "7597ef8731eebdbe898d4b78db51b9db9e33198c";
    ref    = "servant-client-js";
  };


  fusion-plugin-types-src = builtins.fetchGit {
    url   = https://github.com/composewell/fusion-plugin-types.git;
    rev   = "1a7e1c39b4496543b2dc95d59aafbf44041554f1";
    ref   = "v0.1.0";
  };


  streamly-src = builtins.fetchGit {
    url   = https://github.com/composewell/streamly.git;
    rev   = "30ddc28884d1068c75bcb6b865041e42ddd5e5de";
    ref   = "v0.7.2";
  };


  # Additional ignore patterns to keep the Nix src clean
  ignorance = [
    "*.md"
    "figlet"
    "*.nix"
    "*.sh"
    "*.yml"
  ];


  # Get some utilities
  inherit (import (shpadoinkle + "/nix/util.nix") { inherit compiler isJS; }) compilerjs gitignore;


  # Build faster by doing less
  chill = p: (pkgs.haskell.lib.overrideCabal p {
    inherit enableLibraryProfiling enableExecutableProfiling;
  }).overrideAttrs (_: {
    inherit doHoogle doHaddock strictDeps;
  });


  # Overlay containing Shpadoinkle packages, and needed alterations for those packages
  # as well as optimizations from Reflex Platform
  shpadoinkle-overlay =
    import (shpadoinkle + "/nix/overlay.nix") { inherit compiler isJS; };


  # Haskell specific overlay (for you to extend)
  haskell-overlay = hself: hsuper: with pkgs.haskell.lib; {
    abstract-deque-tests = dontCheck hsuper.abstract-deque-tests;
    bsb-http-chunked = dontCheck hsuper.bsb-http-chunked;
    Glob = dontCheck hsuper.Glob;
    http2 = dontCheck hsuper.http2;
    http-date = dontCheck hsuper.http-date;
    iproute = dontCheck hsuper.iproute;
    network-byte-order = dontCheck hsuper.network-byte-order;
    servant-server = dontCheck hsuper.servant-server;
    streaming-commons = dontCheck hsuper.streaming-commons;
    streamly = hself.callCabal2nix "streamly" streamly-src {
      fusion-plugin-types = hself.callCabal2nix "fusion-plugin-types" fusion-plugin-types-src {};
    };
    test-abstract-deque = dontCheck hsuper.test-abstract-deque;
    unix-time = dontCheck hsuper.unix-time;
    wai-app-static = dontCheck hsuper.wai-app-static;
    wai-extra = dontCheck hsuper.wai-extra;
  };


  # Top level overlay (for you to extend)
  snowman-overlay = self: super: {
    haskell = super.haskell //
      { packages = super.haskell.packages //
        { ${compilerjs} = super.haskell.packages.${compilerjs}.override (old: {
            overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) haskell-overlay;
          });
        };
      };
    };


  # Complete package set with overlays applied
  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
    }) {
    overlays = [
      shpadoinkle-overlay
      snowman-overlay
    ];
  };


  # We can name him George
  snowman = pkgs.haskell.packages.${compilerjs}.callCabal2nix "snowman" (gitignore ignorance ./.) {};


in with pkgs; with lib; with haskell.packages.${compiler};

  if inNixShell || asShell
  then shellFor {
    inherit withHoogle;
    packages = _: [snowman];
    COMPILER = compilerjs;
    buildInputs = [ stylish-haskell cabal-install ghcid ];
    shellHook = ''
      ${lolcat}/bin/lolcat ${./figlet}
    '';
  } else chill snowman
