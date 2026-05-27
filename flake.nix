{
  description = "cl-tmux — a tmux-compatible terminal multiplexer in Common Lisp";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # SBCL with every Quicklisp-packaged library the code needs.
        # PTY/termios bindings are pure CFFI + sb-posix — no C files.
        sbclWithDeps = pkgs.sbcl.withPackages (ps: with ps; [
          cffi               # C FFI
          bordeaux-threads   # portable threads + locks
          babel              # string↔octet encoding
        ]);

        cl-tmux = pkgs.stdenv.mkDerivation {
          pname   = "cl-tmux";
          version = "0.1.0";
          src     = ./.;

          buildInputs = [ sbclWithDeps ];

          buildPhase = ''
            export HOME=$TMPDIR
            ${sbclWithDeps}/bin/sbcl \
              --no-sysinit \
              --no-userinit \
              --eval "(require :asdf)" \
              --eval "(push (truename \".\") asdf:*central-registry*)" \
              --eval "(asdf:make :cl-tmux)" \
              --quit
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp cl-tmux $out/bin/
          '';
        };
      in
      {
        packages = {
          default  = cl-tmux;
          inherit cl-tmux;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [ sbclWithDeps ];
          shellHook = ''
            echo "cl-tmux dev shell"
            echo "  sbcl --load cl-tmux.asd --eval '(asdf:load-system :cl-tmux)'"
          '';
        };

        apps.default = {
          type    = "app";
          program = "${cl-tmux}/bin/cl-tmux";
        };
      });
}
