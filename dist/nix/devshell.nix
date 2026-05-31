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

    LLVM_SYS_201_PREFIX = llvm.llvm.dev;
  };
}
