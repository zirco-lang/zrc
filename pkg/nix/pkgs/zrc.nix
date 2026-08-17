{
  naersk,
  self,
  pkgs,
  llvm,
  rust,
  system,
  ...
}:
let
  naerskLib = naersk.lib.${system}.override {
    cargo = rust;
    rustc = rust;
  };
in
naerskLib.buildPackage {
  name = "zrc";
  version = "0.2.0";
  src = ../../..;
  doCheck = true;
  copyLibs = true;

  buildInputs = with pkgs; [
    llvm.llvm
    llvm.libllvm
    llvm.clang
    llvm.lld
    pkg-config
    libffi
    libxml2
    zlib
  ];

  env = {
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
      pkgs.stdenv.cc.cc.lib
      llvm.libllvm
      pkgs.libffi
      pkgs.libxml2
      pkgs.zlib
    ];
  };

  postInstall = ''
    mkdir -p $out/include
    cp -r $src/include/* $out/include/
    cp $src/compiler/libzrc/zrc.h $out/include/
    cp $src/compiler/libzrc/zrc.zh $out/include/
  '';

  LLVM_SYS_221_PREFIX = llvm.llvm.dev;
}
