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
  pname = "zrc";
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
      pkgs.libffi
      pkgs.libxml2
      pkgs.zlib
    ];
  };

  postInstall = ''
    mkdir -p $out/include
    cp -r $src/include/* $out/include/
    cp $src/compiler/libzrc/zrc.h $out/include/
  '';

  setupHook = ../hooks/zrc.sh;

  LLVM_SYS_221_PREFIX = llvm.llvm.dev;
}
