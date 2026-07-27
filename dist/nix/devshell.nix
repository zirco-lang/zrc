{
  pkgs,
  llvm,
  rust,
  ...
}:
{
  default = pkgs.mkShell {
    buildInputs = with pkgs; [
      rust
      cargo-insta
      llvm.llvm
      llvm.libllvm
      llvm.clang
      llvm.lld
      pkg-config
      libffi
      libxml2
      mdbook
      nixfmt
    ];

    LLVM_SYS_221_PREFIX = llvm.llvm.dev;
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
      pkgs.stdenv.cc.cc.lib
      pkgs.libffi
      pkgs.libxml2
      pkgs.zlib
    ];
  };
}
