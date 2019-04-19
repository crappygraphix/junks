{ }:

let
  config = {
    allowUnfree = true;
  };

  inherit (import <nixpkgs> { }) fetchFromGitHub;

  nixpkgs = builtins.fromJSON (builtins.readFile ./reflex-platform.json);

  drv = import (fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    inherit (nixpkgs) rev sha256;
    fetchSubmodules = true;
  }) { inherit config; };
in drv
