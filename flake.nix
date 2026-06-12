{
  description = "The Zirco Programming Language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    naersk.url = "github:nix-community/naersk";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      rust-overlay,
      naersk,
    }:
    let
      inherit (import ./dist/nix/lib.nix { inherit nixpkgs rust-overlay; }) forAllSystems;
    in
    {
      devShells = forAllSystems (args: import ./dist/nix/devshell.nix (inputs // args));
      packages = forAllSystems (args: import ./dist/nix/pkgs/default.nix (inputs // args));
    };
}
