let 
  pkgs = import <nixpkgs> {};
  unstable = import <nixos-unstable> {};
in
{}:
  pkgs.mkShell rec {
    buildInputs = with unstable.pkgs; [
      cargo
      rustc
      rust-analyzer
    ];
  }
