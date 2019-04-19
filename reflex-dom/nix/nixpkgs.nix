{ version ? "17.09" }:

let
  config = {
    allowUnfree = true;
  };

  inherit (import <nixpkgs> { }) fetchFromGitHub;

  nixpkgs = builtins.fromJSON (builtins.readFile (./nixpkgs + "-${version}.json"));

  pkgs = import (fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  }) { inherit config; };
in pkgs
