{
  naersk,
  pkgs,
  llvm,
  rust,
  system,
  ...
}:
let
  naersk' = naersk.lib.${system}.override {
    cargo = rust;
    rustc = rust;
  };
in
naersk'.buildPackage {
  name = "zrc";
  version = "0.2.0";
  src = ../../..;
  doCheck = true;
  copyLibs = true;

  nativeBuildInputs = with pkgs; [
    patchelf
  ];

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
    LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath (
      with pkgs;
      [
        stdenv.cc.cc.lib
        llvm.libllvm
        libffi
        libxml2
        zlib
      ]
    )}";
  };

  postInstall = ''
    mkdir -p $out/include
    cp -r $src/include/* $out/include/
    cp $src/compiler/libzrc/zrc.h $out/include/
  '';

  postFixup = pkgs.lib.optionalString pkgs.stdenv.isLinux ''
    # don't run the patchelf if we're on a dependency phase
    if [ -e "$out/bin/zrc" ]; then
      patchelf \
        --set-rpath ${
          pkgs.lib.makeLibraryPath (
            with pkgs;
            [
              stdenv.cc.cc.lib
              zlib
            ]
          )
        } \
        "$out/bin/"*
    fi
  '';

  LLVM_SYS_221_PREFIX = llvm.llvm.dev;
}
