{
  description = "A command-line tool to manipulate Zsh history files";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils
  }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = nixpkgs.legacyPackages.${system};

    haskellPackages = pkgs.haskellPackages;

    zsh-history =
      haskellPackages.callCabal2nix "zsh-history" self rec {
        # dependency overrides go here
      };
  in {

    packages = {
      inherit zsh-history;
      default = zsh-history;
    };

    devShells.default = pkgs.mkShell {
      buildInputs = [
        haskellPackages.haskell-language-server
        haskellPackages.ghcid
        haskellPackages.cabal-install
      ];

      inputsFrom = builtins.attrValues self.packages.${system};
    };

  });

}
