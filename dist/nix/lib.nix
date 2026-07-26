{ nixpkgs, fenix }:
{
  forAllSystems =
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    in
    f:
    nixpkgs.lib.genAttrs systems (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        llvm = pkgs.llvmPackages_22;
        rust = import ./rust.nix { inherit fenix system; };
      in
      f {
        inherit
          system
          pkgs
          llvm
          rust
          ;
      }
    );
}
