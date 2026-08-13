{
  description = "The Zirco Programming Language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      fenix,
      naersk,
    }:
    let
      inherit (import ./pkg/nix/lib.nix { inherit nixpkgs fenix; }) forAllSystems;
    in
    {
      devShells = forAllSystems (args: import ./pkg/nix/devshell.nix (inputs // args));
      packages = forAllSystems (args: import ./pkg/nix/pkgs/default.nix (inputs // args));
    };
}
