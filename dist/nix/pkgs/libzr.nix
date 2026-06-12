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
  zpkgs = self.packages.${system};
in
pkgs.stdenv.mkDerivation {
  pname = "libzr";
  version = "0.1.0";
  src = ../../../libzr;
  buildInputs = with pkgs; [
    zpkgs.zrc
    llvm.clang
  ];

  buildPhase = ''
    make -C $src all-opt OUTDIR=$PWD/dist ZRC="${zpkgs.zrc}/bin/zrc"
  '';

  installPhase = ''
    mkdir -p $out/lib
    cp dist/libzr.a $out/lib/
    [ -f dist/libzr.so ] && cp dist/libzr.so $out/lib/
    [ -f dist/libzr.dylib ] && cp dist/libzr.dylib $out/lib/
    mkdir -p $out/include
    cp -r include/* $out/include/
  '';

  setupHook = ../hooks/libzr.sh;
}
