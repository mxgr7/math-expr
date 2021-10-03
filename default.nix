# -*- compile-command: "direnv exec . bash -c 'cabal exec -- ghc-pkg list'"; -*-
# -*- compile-command: "nix-shell --run 'cabal exec -- ghc-pkg list'"; -*-
{ pkgs ? import (import ./nix/sources.nix {}).nixpkgs {}, sources ? import ./nix/sources.nix {} }:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix
pkgs.haskellPackages.developPackage {
    root = ./.;
    withHoogle = false;
    returnShellEnv = false;
    overrides = self: _:  {
      yahp    = self.callCabal2nix "yahp" sources.yahp {};
      chronos = self.callCabal2nix "chronos" sources.chronos {};
    };
    modifier = with pkgs.haskell.lib; drv:
      disableLibraryProfiling (dontHaddock (addBuildTools drv
        (with pkgs.haskellPackages; [ cabal-install ghcid])));
  }
