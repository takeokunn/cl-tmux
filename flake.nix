{
  description = "cl-tmux — a tmux-compatible terminal multiplexer in Common Lisp";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Dogfooded sibling libraries, all dependency-free (or cl-prolog-only)
    # ASDF systems.  They are consumed purely as source: their .asd files are
    # placed on ASDF's central registry, so no nixpkgs Lisp-package plumbing
    # is required.
    cl-weave.url       = "github:takeokunn/cl-weave";
    cl-prolog.url      = "github:takeokunn/cl-prolog";
    # flake = false: consumed as a plain source checkout (pushed onto ASDF's
    # central registry below), not through each repo's own flake outputs —
    # keeps this working regardless of whether a given sibling repo ships its
    # own flake.nix.
    cl-cli.url          = "github:nerima-lisp/cl-cli";
    cl-cli.flake        = false;
    cl-boundary-kit.url   = "github:nerima-lisp/cl-boundary-kit";
    cl-boundary-kit.flake = false;
    cl-dataflow.url      = "github:nerima-lisp/cl-dataflow";
    cl-dataflow.flake    = false;
    cl-parser-kit.url    = "github:nerima-lisp/cl-parser-kit";
    cl-parser-kit.flake  = false;
    cl-tty-kit.url       = "github:nerima-lisp/cl-tty-kit";
    cl-tty-kit.flake     = false;
  };

  outputs = { self, nixpkgs, flake-utils, cl-weave, cl-prolog
            , cl-cli, cl-boundary-kit, cl-dataflow, cl-parser-kit, cl-tty-kit }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; config.allowBroken = true; };

        # SBCL with every Quicklisp-packaged library the code depends on.
        # PTY/termios bindings use sb-posix + CFFI against libc — no C files.
        runtimeDeps = ps: with ps; [
          cffi             # C FFI
          bordeaux-threads # portable threads + locks
          babel            # string↔octet encoding
          cl-ppcre         # Perl-compatible regular expressions (#{m/r:...})
        ];
        sbclWithDeps     = pkgs.sbcl.withPackages runtimeDeps;
        # The test suite runs on cl-weave (and cl-prolog for the reasoning
        # specs), both loaded from source via the central registry in the
        # check derivations below — so no extra nixpkgs Lisp packages are
        # needed beyond the runtime deps.  FiveAM is gone.
        sbclWithTestDeps = sbclWithDeps;

        # Every dogfooded sibling library is dependency-free (or depends only
        # on other siblings here), so each is consumed purely as source: push
        # its checkout onto ASDF's central registry rather than packaging it
        # through nixpkgs.  This one list drives every sbcl invocation below.
        siblingRepos = [ cl-prolog cl-weave cl-cli cl-boundary-kit cl-dataflow cl-parser-kit cl-tty-kit ];
        siblingRegistryPushEvals =
          pkgs.lib.concatMapStringsSep " " (repo: ''--eval "(push (truename \"${repo}/\") asdf:*central-registry*)"'')
            siblingRepos;

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
              ${siblingRegistryPushEvals} \
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
        # Run the full suite headlessly on cl-weave (via the FiveAM-surface
        # shim); non-zero exit fails the check.  cl-weave loads from source off
        # the central registry.  PTY tests self-skip when /dev/ptmx is
        # unavailable (sandbox).
        cl-tmux-tests = pkgs.runCommand "cl-tmux-tests"
          {
            nativeBuildInputs = [ sbclWithTestDeps pkgs.tmux cl-tmux ];
            CL_TMUX_COMPAT_BINARY = "${cl-tmux}/bin/cl-tmux";
          }
          ''
            export HOME=$TMPDIR
            cp -r ${./.} ./src-tree
            chmod -R u+w ./src-tree
            cd ./src-tree
            sbcl --no-sysinit --no-userinit \
                 --eval "(require :asdf)" \
                 --eval "(push (truename \".\") asdf:*central-registry*)" \
                 ${siblingRegistryPushEvals} \
                 --eval "(handler-case (asdf:test-system :cl-tmux)
                           (error (e)
                             (format *error-output* \"~&TESTS FAILED: ~A~%\" e)
                             (sb-ext:exit :code 1)))" \
                 --eval "(sb-ext:exit :code 0)"
            touch $out
          '';
        # Run the cl-weave suite for the cl-prolog-backed reasoning read-model.
        # cl-weave and cl-prolog are dependency-free, so they load from source
        # by putting their checkouts on ASDF's central registry alongside the
        # tree.  This is where both dogfooded libraries are exercised together.
        cl-tmux-weave-tests = pkgs.runCommand "cl-tmux-weave-tests"
          {
            nativeBuildInputs = [ sbclWithTestDeps ];
          }
          ''
            export HOME=$TMPDIR
            cp -r ${./.} ./src-tree
            chmod -R u+w ./src-tree
            cd ./src-tree
            sbcl --no-sysinit --no-userinit \
                 --eval "(require :asdf)" \
                 --eval "(push (truename \".\") asdf:*central-registry*)" \
                 ${siblingRegistryPushEvals} \
                 --eval "(handler-case (asdf:test-system :cl-tmux/weave)
                           (error (e)
                             (format *error-output* \"~&WEAVE TESTS FAILED: ~A~%\" e)
                             (sb-ext:exit :code 1)))" \
                 --eval "(sb-ext:exit :code 0)"
            touch $out
          '';
        # Run the cl-weave suite for the cl-dataflow-backed copy-mode
        # lifecycle read-model (src/dataflow/), mirroring cl-tmux-weave-tests.
        cl-tmux-dataflow-tests = pkgs.runCommand "cl-tmux-dataflow-tests"
          {
            nativeBuildInputs = [ sbclWithTestDeps ];
          }
          ''
            export HOME=$TMPDIR
            cp -r ${./.} ./src-tree
            chmod -R u+w ./src-tree
            cd ./src-tree
            sbcl --no-sysinit --no-userinit \
                 --eval "(require :asdf)" \
                 --eval "(push (truename \".\") asdf:*central-registry*)" \
                 ${siblingRegistryPushEvals} \
                 --eval "(handler-case (asdf:test-system :cl-tmux/dataflow)
                           (error (e)
                             (format *error-output* \"~&DATAFLOW TESTS FAILED: ~A~%\" e)
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

        checks = {
          default  = cl-tmux-tests;
          weave    = cl-tmux-weave-tests;
          dataflow = cl-tmux-dataflow-tests;
        };

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
          meta = {
            description = "cl-tmux — a tmux-compatible terminal multiplexer in Common Lisp";
            mainProgram  = "cl-tmux";
          };
        };
      });
}
