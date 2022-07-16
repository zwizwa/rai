with import <nixpkgs> {};
mkShell {
  buildInputs = [
    gcc racket

    # keep this line if you use bash
    bashInteractive
  ];
}
