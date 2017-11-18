with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "rai-current";
  buildInputs = [ racket ];
}

