let 
  pkgs = import <nixpkgs> {};
  unstable = import <nixos-unstable> {};
in
{}:
  pkgs.mkShell rec {
    buildInputs = [
      unstable.pkgs.cargo
      unstable.pkgs.rustc
    ];
  }