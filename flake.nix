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
        runtimeDeps = ps: with ps; [
          cffi             # C FFI
          bordeaux-threads # portable threads + locks
          babel            # string↔octet encoding
        ];
        sbclWithDeps     = pkgs.sbcl.withPackages runtimeDeps;
        # Test build also needs the FiveAM test framework.
        sbclWithTestDeps = pkgs.sbcl.withPackages
          (ps: (runtimeDeps ps) ++ [ ps.fiveam ]);

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
            # --noinform is a C-runtime option; it must precede --core.
            # --no-sysinit/userinit are Lisp options; they follow --core.
            makeWrapper ${sbclWithDeps}/bin/sbcl $out/bin/cl-tmux \
              --add-flags "--noinform --core $out/lib/cl-tmux/cl-tmux.core --no-sysinit --no-userinit"
          '';
        };
        # Run the FiveAM suite headlessly; non-zero exit fails the check.
        # PTY tests self-skip when /dev/ptmx is unavailable (sandbox).
        cl-tmux-tests = pkgs.runCommand "cl-tmux-tests"
          { nativeBuildInputs = [ sbclWithTestDeps ]; }
          ''
            export HOME=$TMPDIR
            cp -r ${./.} ./src-tree
            chmod -R u+w ./src-tree
            cd ./src-tree
            sbcl --no-sysinit --no-userinit \
                 --eval "(require :asdf)" \
                 --eval "(push (truename \".\") asdf:*central-registry*)" \
                 --eval "(handler-case (asdf:test-system :cl-tmux)
                           (error (e)
                             (format *error-output* \"~&TESTS FAILED: ~A~%\" e)
                             (sb-ext:exit :code 1)))" \
                 --eval "(sb-ext:exit :code 0)"
            touch $out
          '';
      in
      {
        packages = {
          default  = cl-tmux;
          inherit cl-tmux;
        };

        checks.default = cl-tmux-tests;

        devShells.default = pkgs.mkShell {
          buildInputs = [ sbclWithTestDeps ];
          shellHook = ''
            echo "cl-tmux dev shell"
            echo "  sbcl --load cl-tmux.asd --eval '(asdf:load-system :cl-tmux)'"
            echo "  run tests: sbcl --eval '(asdf:test-system :cl-tmux)' --quit"
          '';
        };

        apps.default = {
          type    = "app";
          program = "${cl-tmux}/bin/cl-tmux";
        };
      });
}
