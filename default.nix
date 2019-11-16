{}:
let
  pkgs = import ../nixpkgs {};
  racket = pkgs.callPackage ../../nix/nixpkgs-extra/racket {};
in
  pkgs.stdenv.mkDerivation {
  name = "rai-current";
  buildInputs = with pkgs; [ racket which ];
}

