let 
  pkgs = import <nixpkgs> {};
  unstable = import <nixos-unstable> {};
in
{}:
  pkgs.mkShell {
    buildInputs = with unstable.pkgs; [
      cargo
      rustc
      cargo-watch
      rustfmt
      #rust-analyzer
      clippy
      lldb
      gdb
      vscode-extensions.vadimcn.vscode-lldb.adapter
    ];
  }
