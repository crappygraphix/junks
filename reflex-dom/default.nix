# https://github.com/ElvishJerricco/reflex-project-skeleton
# https://github.com/reflex-frp/reflex-platform/blob/develop/project/default.nix

{ reflex-platform ? import ./nix/reflex-platform.nix { }
, nixpkgs ? import ./nix/nixpkgs.nix { version = "unstable"; }
, }:

reflex-platform.project ({ pkgs, ... }: {
  packages = {
    junks-frontend = ./.;
  };

  overrides = self: super: {
    http-media = pkgs.haskell.lib.dontCheck super.http-media;
    servant-reflex = pkgs.haskell.lib.dontHaddock (
      pkgs.haskell.lib.doJailbreak super.servant-reflex
    );
  };

  # android.frontend = {
  #   executableName = "junks-frontend";
  #   applicationId = "org.example.frontend";
  #   displayName = "Example Android App";
  # };

  # ios.frontend = {
  #   executableName = "junks-frontend";
  #   bundleIdentifier = "org.example.frontend";
  #   bundleName = "Example iOS App";
  # };

  shells = {
    ghc = [
      "junks-frontend"
    ];
    ghcjs = [
      "junks-frontend"
    ];
  };

  shellToolOverrides = ghc: super: {
    # current nixpkgs in reflex-platform is 2.0.0.1, we want >= 2.2.0.0
    cabal-install = nixpkgs.haskellPackages.cabal-install;
  };
})
