args@{
  naersk,
  self,
  pkgs,
  llvm,
  rust,
  system,
  ...
}:
{
  zrc = import ./zrc.nix args;
  libzr = import ./libzr.nix args;
  default = self.packages.${system}.zrc;
}
