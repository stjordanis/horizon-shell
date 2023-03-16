{
  description = "horizon-shell";
  inputs = {
    get-flake.url = "github:ursi/get-flake";
    flake-utils.url = "github:numtide/flake-utils";
    horizon-platform.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform";
    lint-utils = {
      url = "git+https://gitlab.homotopic.tech/nix/lint-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };
  outputs =
    inputs@
    { self
    , get-flake
    , flake-utils
    , horizon-platform
    , lint-utils
    , nixpkgs
    , ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    with pkgs.haskell.lib;
    with pkgs.writers;
    with pkgs.lib;
    let

      horizon-shell = import ./default.nix {
        haskellPackages = horizon-platform.legacyPackages.${system};
        inherit (pkgs) runCommand writeShellScriptBin;
        inherit (pkgs.lib) splitString;
      };
    in
    {

      apps = {

        default = {
          type = "app";
          program = "${horizon-shell}/bin/horizon-shell";
        };

      };

      checks =
        with lint-utils.outputs.linters.${system}; {
          dhall-format = dhall-format { src = self; };
          nixpkgs-fmt = nixpkgs-fmt { src = self; };
          stylish-haskell = stylish-haskell { src = self; };
        };

      packages.default = horizon-shell;

    });
}
