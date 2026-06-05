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
    cp -r $src/include $out/include
  '';

  setupHook = ../hooks/zrc.sh;

  LLVM_SYS_221_PREFIX = llvm.llvm.dev;
}
