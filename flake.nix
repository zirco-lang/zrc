{
  description = "The Zirco Programming Language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };
  
  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        overlays = [ rust-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; }
        llvm = pkgs.llvmPackages_20;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            rust-bin.stable.latest.default
            rust-bin.nightly.latest.default
            llvm.llvm
            llvm.libllvm
            llvm.clang
            llvm.lld
            pkg-config
            libffi
            libxml2
          ];

          LLVM_SYS_201_PREFIX = llvm.llvm.dev;
        };
      }
    );
}
