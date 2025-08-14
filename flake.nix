{
  description = "Widgetmancer dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    #stable.url = "github:NixOS/nixpkgs/nixos-23.05";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 

        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # rustup
            # (rust-bin.stable.latest.default.override { 
            (rust-bin.nightly.latest.default.override { 
              extensions = [ 
                "llvm-tools-preview" # for code coverage
              ];
            })
            # cargo
            # rustc
            # cargo-watch
            # rustfmt
            rust-analyzer
            clippy
            # lldb
            # gdb
            bacon
            cargo-flamegraph
            cargo-profiler
            cargo-nextest
            cargo-limit
            cargo-llvm-cov
            # luarocks
            uftrace
          ];
          shellHook = "zsh";
        };
      });
}

