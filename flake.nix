{
  description = "The Zirco Programming Language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    naersk.url = "github:nix-community/naersk";
  };
  
  outputs = { self, nixpkgs, flake-utils, rust-overlay, naersk }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        overlays = [ rust-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
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

        packages.zrc =
          let naerskLib = naersk.lib.${system}.override {
            cargo = pkgs.rust-bin.stable.latest.default;
            rustc = pkgs.rust-bin.stable.latest.default;
          };
          in naerskLib.buildPackage {
            pname = "zrc";
            src = ./.;
            buildInputs = with pkgs; [
              rust-bin.stable.latest.default
              llvm.llvm
              llvm.libllvm
              llvm.clang
              llvm.lld
              pkg-config
              libffi
              libxml2
            ];
            postInstall = ''
		cp -r $src/include $out
            '';
	    LLVM_SYS_201_PREFIX = llvm.llvm.dev;
         };

        packages.default = self.packages.${system}.zrc;
      }
    );
}
