{

  description = "horizon-shell";

  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {
    crazyshell.url = "git+https://gitlab.horizon-haskell.net/shells/crazyshell";
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
    , crazyshell
    , flake-utils
    , horizon-platform
    , lint-utils
    , nixpkgs
    , ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      with crazyshell.lib;
      let
        pkgs = import nixpkgs { inherit system; };

        haskellPackages = horizon-platform.legacyPackages.${system};

        horizon-shell = import ./default.nix {
          inherit pkgs haskellPackages mkCrazyShell;
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
