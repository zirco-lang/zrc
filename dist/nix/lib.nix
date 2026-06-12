{ nixpkgs, rust-overlay }:
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
        overlays = [ rust-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
        llvm = pkgs.llvmPackages_22;
        rust = pkgs.rust-bin.nightly.latest.default.override {
          extensions = [
            "rust-src"
            "rust-analyzer"
          ];
        };
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
