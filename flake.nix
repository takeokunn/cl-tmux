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

        # SBCL with every Quicklisp-packaged library the code depends on.
        # PTY/termios bindings use sb-posix + CFFI against libc — no C files.
        sbclWithDeps = pkgs.sbcl.withPackages (ps: with ps; [
          cffi             # C FFI
          bordeaux-threads # portable threads + locks
          babel            # string↔octet encoding
        ]);

        cl-tmux = pkgs.stdenv.mkDerivation {
          pname   = "cl-tmux";
          version = "0.1.0";
          src     = ./.;

          nativeBuildInputs = [ pkgs.makeWrapper ];
          buildInputs       = [ sbclWithDeps ];

          buildPhase = ''
            export HOME=$TMPDIR

            # Compile all Lisp sources and save the image as a core file.
            # Using save-lisp-and-die without :executable avoids the
            # macOS-specific issue where embedded-core binaries fail to
            # find sbcl.core at runtime.
            ${sbclWithDeps}/bin/sbcl \
              --no-sysinit \
              --no-userinit \
              --noinform \
              --eval "(require :asdf)" \
              --eval "(push (truename \".\") asdf:*central-registry*)" \
              --eval "(asdf:load-system :cl-tmux)" \
              --eval "(sb-ext:save-lisp-and-die \"cl-tmux.core\"
                         :toplevel #'cl-tmux:main
                         :executable nil
                         :compression t)" \
              --quit
          '';

          installPhase = ''
            mkdir -p $out/lib/cl-tmux $out/bin

            # Install the compressed Lisp core.
            cp cl-tmux.core $out/lib/cl-tmux/

            # Wrap sbcl so users just call "cl-tmux".
            makeWrapper ${sbclWithDeps}/bin/sbcl $out/bin/cl-tmux \
              --add-flags "--noinform --no-sysinit --no-userinit --core $out/lib/cl-tmux/cl-tmux.core"
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
            echo "  or: sbcl --core cl-tmux.core   (after asdf:make)"
          '';
        };

        apps.default = {
          type    = "app";
          program = "${cl-tmux}/bin/cl-tmux";
        };
      });
}
